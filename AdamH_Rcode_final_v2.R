# FINAL DATA VISUALIZATION PROJECT

library("tidyverse")
library("plotly")
library("RColorBrewer")
library("tidyverse")
library("data.table")


############## Section 1 get and transform the data #############

data_path <- file.path("C:/Users/ahilgenkamp/Documents/Grad School/Data Visualization/Project/raw-data")
#data_path <- file.path("raw-data")
print(data_path)

#termInfo <- read_rds(file.path(data_path, "DimStudentTermInfo.RDS"))
#View(termInfo)

fact_ite <- read_rds(file.path(data_path, "clean_students_sample.RDS"))
fact_ite <- fact_ite %>%
  group_by(CurrentCSN) %>%
  count() %>%
  group_by(n) %>%
  sample_frac(size = 0.01) %>%
  ungroup() %>%
  select(CurrentCSN) %>%
  inner_join(fact_ite)

#View(fact_ite)

fact_degree <- read_rds(file.path(data_path, "clean_degree_sample.RDS"))

#View(fact_degree)

just_degree <- fact_degree %>%
  mutate(CurrentCSN = as.numeric(CurrentCSN)) %>%
  select(CurrentCSN, AcademicYearKey) %>%
  distinct() %>%
  mutate(has_degree = TRUE)

# View(just_degree)

ite_degree <- fact_ite %>%
  mutate(CurrentCSN = as.numeric(CurrentCSN)) %>%
  select(-contains("Credit"), -termname) %>%
  group_by(CurrentCSN, AcademicYearKey) %>%
  slice(1) %>%
  mutate_at(vars(ends_with("Key")), as.integer) %>%
  left_join(
    just_degree,
    by = c("CurrentCSN", "AcademicYearKey")
  ) %>%
  group_by(CurrentCSN) %>%
  arrange(CurrentCSN, AcademicYearKey) %>%
  mutate(has_degree = coalesce(has_degree, FALSE)) %>%
  mutate(has_degree = as.logical(cumsum(has_degree)))

# View(ite_degree)

#transitions for test
consolidated_terms <- read_rds(file.path(data_path, "consolidated_terms.RDS"))
dim_term <- read_rds(file.path(data_path, "DimTerm.RDS"))

consolidated_terms <- consolidated_terms %>%
  filter(AcademicYearKey >= min(fact_ite[["AcademicYearKey"]])) %>%
  filter(AcademicYearKey <= max(fact_ite[["AcademicYearKey"]])) %>%
  filter(termname != "Summer") %>%
  select(-TermKey, -termname) %>%
  distinct() %>%
  arrange(AcademicYearKey) %>%
  mutate(rown = row_number()) %>%
  print()


transitions <- consolidated_terms %>%
  mutate(next_term = lead(rown)) %>%
  inner_join(
    consolidated_terms,
    by = c("next_term" = "rown"),
    suffix = c("_before", "_after")
  ) %>%
  select(-rown, -next_term)

#View(transitions)


#View(ite_degree)


#Edit the data to get it ready for the chart
student_transitions <- transitions %>%
  crossing(CurrentCSN = ite_degree[["CurrentCSN"]]) %>%
  left_join(
    ite_degree,
    by = c(
      "CurrentCSN" = "CurrentCSN",
      "AcademicYearKey_before" = "AcademicYearKey"
    )
  ) %>%
  arrange(CurrentCSN, AcademicYearKey_before) %>%
  group_by(CurrentCSN) %>%
  distinct() %>%
  mutate(terms_enrolled = cumsum(!is.na(LocationKey))) %>%
  filter(terms_enrolled > 0) %>%
  left_join(
    ite_degree,
    by = c(
      "CurrentCSN" = "CurrentCSN",
      "AcademicYearKey_after" = "AcademicYearKey"
    ),
    suffix = c("_before", "_after")
  ) %>%
  mutate(has_degree = as.logical(
    coalesce(has_degree_before, FALSE) + coalesce(has_degree_after, FALSE)
  )) %>%
  distinct() %>%
  select(-has_degree_before, - has_degree_after) %>%
  arrange(CurrentCSN, AcademicYearKey_before) %>%
  mutate(
    term_sequence = row_number(),
    has_degree = as.logical(cumsum(case_when(
      is.na(has_degree) ~ FALSE,
      1 == 1 ~ has_degree
    )))
  ) %>%
  mutate_at(
    vars(contains("Key_")),
    funs(
      case_when(
        is.na(.) & has_degree ~ 9999L,
        is.na(.) ~ 0L,
        1 == 1 ~ .
      )
    )) %>%
  select(-has_degree)


#View(student_transitions)

#################### ALEX CHARTS ###############################

dim_location <- read_rds(file.path(data_path, "DimLocation.RDS"))
clean_locations <- dim_location %>%
  bind_rows(tibble(LocationKey = c(0L, 9999L))) %>%
  select(
    LocationKey, SystemName, InstitutionName, CampusName,
    InstitutionControl,
    CampusState
  ) %>%
  mutate(
    CampusState = case_when(
      LocationKey == 0 ~ "Unenrolled",
      LocationKey == 9999 ~ "Graduated",
      CampusState == "IN" ~ "IN",
      1 == 1 ~ "Out of State"
    )
  ) %>%
  mutate(
    SystemName = case_when(
      LocationKey == 0 ~ "Unenrolled",
      LocationKey == 9999 ~ "Graduated",
      CampusState != "IN" ~ "Out of State",
      InstitutionControl != "Public" ~ "Private Institution",
      1 == 1 ~ SystemName
    )
  ) %>%
  mutate(
    InstitutionName = case_when(
      LocationKey == 0 ~ "Unenrolled",
      LocationKey == 9999 ~ "Graduated",
      CampusState != "IN" ~ "Out of State",
      InstitutionControl != "Public" ~ "Private Institution",
      1 == 1 ~ InstitutionName
    )
  ) %>%
  mutate(
    CampusName = case_when(
      LocationKey == 0 ~ "Unenrolled",
      LocationKey == 9999 ~ "Graduated",
      CampusState != "IN" ~ "Out of State",
      InstitutionControl != "Public" ~ "Private Institution",
      1 == 1 ~ CampusName
    )
  ) %>%
  mutate(
    SystemColor = case_when(
      SystemName == "Private Institution" ~ "#d5b82a",
      SystemName == "Ball State University" ~ "#ba0c2f",
      SystemName == "Purdue University System" ~ "#c28e0e",
      SystemName == "Indiana University System" ~ "#7A1705",
      SystemName == "University of Southern Indiana" ~ "#002856",
      SystemName == "Indiana State University" ~ "#053969",
      SystemName == "Vincennes University" ~ "#1b1e5a",
      SystemName == "Ivy Tech Community College" ~ "#086c54",
      SystemName == "Out of State" ~ "grey25",
      SystemName == "Unenrolled" ~ "grey50",
      SystemName == "Graduated" ~ "#336699",
      1 == 1 ~ as.character(NA)
    )
  ) %>%
  mutate_if(is.character, factor, ordered = FALSE) %>%
  select(-InstitutionControl) %>%
  distinct()


#INTER SYSTEM TRANSFER

location_transitions <- student_transitions %>%
  select(term_sequence, CurrentCSN, LocationKey_before, LocationKey_after) %>%
  inner_join(
    clean_locations,
    by = c("LocationKey_before" = "LocationKey")
  ) %>%
  inner_join(
    clean_locations,
    by = c("LocationKey_after" = "LocationKey"),
    suffix = c("_before", "_after")
  ) %>%
  print()


system_summary <- location_transitions %>%
  group_by(term_sequence, SystemName_before, SystemName_after) %>%
  summarise(n = n_distinct(CurrentCSN)) %>%
  filter(term_sequence < 6) %>%
  group_by(term_sequence) %>%
  mutate(
    source_label = paste0(SystemName_before, ": Term ", term_sequence),
    to_label = paste0(SystemName_after, ": Term ", term_sequence + 1)
  ) %>%
  mutate(pct = n / sum(n)) #%>%

label_list <- union(
  system_summary[["source_label"]],
  system_summary[["to_label"]]
)
label_colors <- tibble(SystemName = label_list) %>%
  mutate(
    SystemName = gsub(x = SystemName,
                      pattern = ": Term .*",
                      replacement = "")
  ) %>%
  inner_join(
    clean_locations %>%
      select(SystemName, SystemColor) %>%
      distinct() %>%
      mutate_all(as.character),
    by = "SystemName"
  ) %>%
  pull(SystemColor)


p <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = label_list,
    color = label_colors,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0
    )
  ),
  link = list(
    source = match(system_summary$source_label, label_list) - 1,
    target = match(system_summary$to_label, label_list) - 1,
    # target = system_summary$to_label,
    value =  system_summary$pct * 100
  )
) %>%
  layout(
    title = "Inter-System Transfer in Indiana",
    # width = 800,
    # height = 400,
    font = list(
      size = 10
    )
  )
print(p)


# IU SYSTEM TRANSFERS 

iu_summary <- location_transitions %>%
  filter(
    SystemName_before == "Indiana University System" | 
      SystemName_after == "Indiana University System"
  ) %>%
  mutate(InstitutionName_before = case_when(
    SystemName_before == "Indiana University System" ~ as.character(InstitutionName_before),
    SystemName_before == "Unenrolled" ~ "Unenrolled",
    SystemName_before == "Graduated" ~ "Graduated",
    SystemName_before == "Out of State" ~ "Out of State",
    1 == 1 ~ "Other Institution"
  )) %>%
  mutate(InstitutionName_after = case_when(
    SystemName_after == "Indiana University System" ~ as.character(InstitutionName_after),
    SystemName_after == "Unenrolled" ~ "Unenrolled",
    SystemName_after == "Graduated" ~ "Graduated",
    SystemName_after == "Out of State" ~ "Out of State",
    1 == 1 ~ "Other Institution"
  )) %>%
  group_by(term_sequence, InstitutionName_before, InstitutionName_after) %>%
  summarise(n = n_distinct(CurrentCSN)) %>%
  filter(term_sequence < 6) %>%
  group_by(term_sequence) %>%
  mutate(
    source_label = paste0(InstitutionName_before, ": Term ", term_sequence),
    to_label = paste0(InstitutionName_after, ": Term ", term_sequence + 1)
  ) %>%
  mutate(pct = n / sum(n)) #%>%

label_list <- union(
  iu_summary[["source_label"]],
  iu_summary[["to_label"]]
)
label_colors <- tibble(InstitutionName = label_list) %>%
  mutate(
    InstitutionName = gsub(x = InstitutionName,
                           pattern = ": Term .*",
                           replacement = "")
  ) %>%
  left_join(
    clean_locations %>%
      select(InstitutionName, SystemColor) %>%
      distinct() %>%
      mutate_all(as.character),
    by = "InstitutionName"
  ) %>%
  pull(SystemColor)


p <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = label_list,
    color = label_colors,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0
    )
  ),
  link = list(
    source = match(iu_summary$source_label, label_list) - 1,
    target = match(iu_summary$to_label, label_list) - 1,
    # target = iu_summary$to_label,
    value =  iu_summary$pct * 100
  )
) %>%
  layout(
    title = "Transfer within the IU System",
    # width = 800,
    # height = 400,
    font = list(
      size = 10
    )
  )
print(p)


# GPA TRENDS 

gpa_summary <- student_transitions %>%
  mutate(
    CumulativeGPA_before = case_when(
      LocationKey_before == 9999L ~ 9999,
      LocationKey_before == 0L ~ -1,
      1 == 1 ~ CumulativeGPA_before
    ),
    CumulativeGPA_after = case_when(
      LocationKey_after == 9999L ~ 9999,
      LocationKey_after == 0L ~ -1,
      1 == 1 ~ CumulativeGPA_after
    )
  ) %>%
  mutate_at(vars(starts_with("CumulativeGPA")),
            funs(factor(case_when(
              . == 9999 ~ "Graduated",
              . == -1 ~ "Unenrolled",
              . > 3.5 ~ "> 3.5",
              . > 3.0 ~ "3-3.5",
              . > 2.5 ~ "2.5-3",
              . > 2.0 ~ "2-2.5",
              1 == 1 ~ "< 2"
            ),
            levels = c("Graduated", "> 3.5", "3-3.5", "2.5-3", "2-2.5", "< 2", "Unenrolled"),
            ordered = TRUE
            ))
  ) %>%
  group_by(term_sequence, CumulativeGPA_before, CumulativeGPA_after) %>%
  summarise(n = n_distinct(CurrentCSN)) %>%
  filter(term_sequence < 6) %>%
  group_by(term_sequence) %>%
  mutate(
    source_label = paste0(CumulativeGPA_before, ": Term ", term_sequence),
    to_label = paste0(CumulativeGPA_after, ": Term ", term_sequence + 1)
  ) %>%
  mutate(pct = n / sum(n)) #%>%

label_list <- union(
  gpa_summary[["source_label"]],
  gpa_summary[["to_label"]]
)
label_colors <- tibble(CumulativeGPA = label_list) %>%
  mutate(
    CumulativeGPA = gsub(x = CumulativeGPA,
                         pattern = ": Term .*",
                         replacement = "")
  ) %>%
  mutate(
    SystemColor = RColorBrewer::brewer.pal(8, "Set2")[as.numeric(factor(CumulativeGPA))]
  ) %>%
  pull(SystemColor)


p <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = label_list,
    color = label_colors,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0
    )
  ),
  link = list(
    source = match(gpa_summary$source_label, label_list) - 1,
    target = match(gpa_summary$to_label, label_list) - 1,
    # target = iu_summary$to_label,
    value =  gpa_summary$pct * 100
  )
) %>%
  layout(
    title = "GPA Flow",
    # width = 800,
    # height = 400,
    font = list(
      size = 10
    )
  )
print(p)

# DIFFERENT STACKING 

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = label_list,
    color = label_colors,
    pad = 0,
    thickness = 15,
    line = list(
      color = "black",
      width = 0
    )
  ),
  link = list(
    source = match(gpa_summary$source_label, label_list) - 1,
    target = match(gpa_summary$to_label, label_list) - 1,
    # target = iu_summary$to_label,
    value =  gpa_summary$pct * 100
  )
) %>%
  layout(
    title = "GPA Flow",
    # width = 800,
    # height = 400,
    font = list(
      size = 10
    )
  )
print(p)


################ ADAM CHARTS #######################

######### Get program Data to show how students switch majors ##############

dim_program <- read_rds(file.path(data_path, "DimProgramMajor.RDS"))

#View(dim_program)


clean_program <- dim_program %>%
  bind_rows(tibble(ProgramMajorKey = c(0L, 9999L))) %>%
  filter( (ProgramLevel == 'Undergraduate' & ProgramDegreeLevelGroup == 'Bachelor\'s') | ProgramMajorKey %in% c(0,9999) ) %>%
  select(
    ProgramMajorKey, ProgramCIPCategory
  ) %>%
  mutate(
    ProgramCIPCategory = case_when(
      ProgramMajorKey == 0 ~ "Unenrolled",
      ProgramMajorKey == 9999 ~ "Graduated",
      ProgramCIPCategory == "Science, Technology, Engineering, and Math (STEM)" ~ "STEM",
      ProgramCIPCategory == "Social and Behavioral Sciences and Human Services" ~ "Social Sciences & Human Services",
      1 == 1 ~ ProgramCIPCategory
    )
  ) %>%
  mutate(
    programColor = case_when(
      ProgramCIPCategory == "Arts and Humanities" ~ "#1b9e77",
      ProgramCIPCategory == "Business and Communication" ~ "#d95f02",
      ProgramCIPCategory == "Education" ~ "#7570b3",
      ProgramCIPCategory == "STEM" ~ "#e7298a",
      ProgramCIPCategory == "Social Sciences & Human Services" ~ "#66a61e",
      ProgramCIPCategory == "Health" ~ "#e6ab02",
      ProgramCIPCategory == "Trades" ~ "#a6761d",
      ProgramCIPCategory == "Undecided" ~ "#666666",
      ProgramCIPCategory == "Unenrolled" ~ "grey50",
      ProgramCIPCategory == "Graduated" ~ "#336699",
      1 == 1 ~ as.character(NA)
    )
  ) %>%
  mutate_if(is.character, factor, ordered = FALSE) %>%
  distinct()

#View(clean_program)


program_transitions <- student_transitions %>%
  select(term_sequence, CurrentCSN, ProgramMajorKey_before, ProgramMajorKey_after) %>%
  inner_join(
    clean_program,
    by = c("ProgramMajorKey_before" = "ProgramMajorKey")
  ) %>%
  inner_join(
    clean_program,
    by = c("ProgramMajorKey_after" = "ProgramMajorKey"),
    suffix = c("_before", "_after")
  )

#View(program_transitions)


program_summary <- program_transitions %>%
  group_by(term_sequence, ProgramCIPCategory_before, ProgramCIPCategory_after) %>%
  summarise(n = n_distinct(CurrentCSN)) %>%
  filter(term_sequence < 5) %>%
  group_by(term_sequence) %>%
  mutate(
    source_label = paste0(ProgramCIPCategory_before, ": Year ", term_sequence),
    to_label = paste0(ProgramCIPCategory_after, ": Year ", term_sequence + 1)
  ) %>%
  mutate(pct = n / sum(n)) #%>%


#View(program_summary)

label_list <- union(
  program_summary[["source_label"]],
  program_summary[["to_label"]]
)
label_colors <- tibble(ProgramCIPCategory = label_list) %>%
  mutate(
    ProgramCIPCategory = gsub(x = ProgramCIPCategory,
                              pattern = ": Year .*",
                              replacement = "")
  ) %>%
  inner_join(
    clean_program %>%
      select(ProgramCIPCategory, programColor) %>%
      distinct() %>%
      mutate_all(as.character),
    by = "ProgramCIPCategory"
  ) %>%
  pull(programColor)


#plot
p <- plot_ly(
  type = "sankey",
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "%",
  node = list(
    label = label_list,
    color = label_colors,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0
    )
  ),
  link = list(
    source = match(program_summary$source_label, label_list) - 1,
    target = match(program_summary$to_label, label_list) - 1,
    value =  program_summary$pct * 100
  )
) %>%
  layout(
    title = "Undergraduate Academic Program Changes in Indiana",
    font = list(
      size = 10
    )
  )
print(p)


########## FOCUS IN ON ONE GROUP ##############

program_transitions <- student_transitions %>%
  select(term_sequence, CurrentCSN, ProgramMajorKey_before, ProgramMajorKey_after) %>%
  inner_join(
    clean_program,
    by = c("ProgramMajorKey_before" = "ProgramMajorKey")
  ) %>%
  inner_join(
    clean_program,
    by = c("ProgramMajorKey_after" = "ProgramMajorKey"),
    suffix = c("_before", "_after")
  )

#View(program_transitions)

oneStart <- program_transitions %>%
  filter(ProgramCIPCategory_before == 'Undecided' & term_sequence == '1') %>%
  inner_join(
    program_transitions,
    by = c("CurrentCSN" = "CurrentCSN")
  )

#View(oneStart)

program_summary <- oneStart %>%
  select(CurrentCSN, term_sequence = term_sequence.y, ProgramCIPCategory_before = ProgramCIPCategory_before.y, 
         ProgramCIPCategory_after = ProgramCIPCategory_after.y) %>%
  group_by(term_sequence, ProgramCIPCategory_before, ProgramCIPCategory_after) %>%
  summarise(n = n_distinct(CurrentCSN)) %>%
  filter(term_sequence < 5) %>%
  group_by(term_sequence) %>%
  mutate(
    source_label = paste0(ProgramCIPCategory_before, ": Year ", term_sequence),
    to_label = paste0(ProgramCIPCategory_after, ": Year ", term_sequence + 1)
  ) %>%
  mutate(pct = n / sum(n)) %>%
  filter(source_label != 'Graduated: Year 5')


#View(program_summary)

label_list <- union(
  program_summary[["source_label"]],
  program_summary[["to_label"]]
)
label_colors <- tibble(ProgramCIPCategory = label_list) %>%
  mutate(
    ProgramCIPCategory = gsub(x = ProgramCIPCategory,
                              pattern = ": Year .*",
                              replacement = "")
  ) %>%
  inner_join(
    clean_program %>%
      select(ProgramCIPCategory, programColor) %>%
      distinct() %>%
      mutate_all(as.character),
    by = "ProgramCIPCategory"
  ) %>%
  pull(programColor)


#plot
p <- plot_ly(
  type = "sankey",
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "%",
  node = list(
    label = label_list,
    color = label_colors,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0
    )
  ),
  link = list(
    source = match(program_summary$source_label, label_list) - 1,
    target = match(program_summary$to_label, label_list) - 1,
    value =  program_summary$pct * 100
  )
) %>%
  layout(
    title = "Path of a typical Undecided Student in Indiana",
    font = list(
      size = 10
    )
  )
print(p)


########## FOCUS IN ON ONE SCHOOL ##############

location_program_transitions <- student_transitions %>%
  select(term_sequence, CurrentCSN, LocationKey_before, LocationKey_after, ProgramMajorKey_before, ProgramMajorKey_after) %>%
  inner_join(
    clean_locations,
    by = c("LocationKey_before" = "LocationKey")
  ) %>%
  inner_join(
    clean_locations,
    by = c("LocationKey_after" = "LocationKey"),
    suffix = c("_before", "_after")
  ) %>%
  inner_join(
    clean_program,
    by = c("ProgramMajorKey_before" = "ProgramMajorKey")
  ) %>%
  inner_join(
    clean_program,
    by = c("ProgramMajorKey_after" = "ProgramMajorKey"),
    suffix = c("_before", "_after")
  )

#View(location_program_transitions)


oneStart <- location_program_transitions %>%
  filter(InstitutionName_before == 'Indiana University-Bloomington' & term_sequence == '1') %>%
  inner_join(
    program_transitions,
    by = c("CurrentCSN" = "CurrentCSN")
  )

#View(oneStart)

program_summary <- oneStart %>%
  select(CurrentCSN, term_sequence = term_sequence.y, ProgramCIPCategory_before = ProgramCIPCategory_before.y, 
         ProgramCIPCategory_after = ProgramCIPCategory_after.y) %>%
  group_by(term_sequence, ProgramCIPCategory_before, ProgramCIPCategory_after) %>%
  summarise(n = n_distinct(CurrentCSN)) %>%
  filter(term_sequence < 5) %>%
  group_by(term_sequence) %>%
  mutate(
    source_label = paste0(ProgramCIPCategory_before, ": Year ", term_sequence),
    to_label = paste0(ProgramCIPCategory_after, ": Year ", term_sequence + 1)
  ) %>%
  mutate(pct = n / sum(n)) #%>%
#filter(source_label != 'Graduated: Year 5')


#View(program_summary)

label_list <- union(
  program_summary[["source_label"]],
  program_summary[["to_label"]]
)
label_colors <- tibble(ProgramCIPCategory = label_list) %>%
  mutate(
    ProgramCIPCategory = gsub(x = ProgramCIPCategory,
                              pattern = ": Year .*",
                              replacement = "")
  ) %>%
  inner_join(
    clean_program %>%
      select(ProgramCIPCategory, programColor) %>%
      distinct() %>%
      mutate_all(as.character),
    by = "ProgramCIPCategory"
  ) %>%
  pull(programColor)


#plot
p <- plot_ly(
  type = "sankey",
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "%",
  node = list(
    label = label_list,
    color = label_colors,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0
    )
  ),
  link = list(
    source = match(program_summary$source_label, label_list) - 1,
    target = match(program_summary$to_label, label_list) - 1,
    value =  program_summary$pct * 100
  )
) %>%
  layout(
    title = "Path of a typical Indiana University - Bloomington Student",
    font = list(
      size = 10
    )
  )
print(p)




