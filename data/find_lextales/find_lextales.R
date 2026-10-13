# Read in the data
completed_lextale = read.csv(here("data" ,"find_lextales", "comp_lex.csv")) %>% 
  filter(Status == "APPROVED") 

completed_exp = read.csv(here("data" ,"find_lextales", "comp_exp.csv")) %>% 
  filter(Status == "APPROVED") 

both <- completed_exp %>%
  semi_join(completed_lextale, by = "Participant.id")

missing_exp = completed_lextale %>% 
  filter(!Participant.id %in% both$Participant.id) 

missing_lextale = completed_exp %>% 
  filter(!Participant.id %in% both$Participant.id) 


lextale_files <- fs::dir_ls(here("data", "find_lextales", "lextale"),
                            regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) 

exp <- fs::dir_ls(here("data", "find_lextales", "exp_task"),
                  regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) 

missing_lextale = completed_exp %>% # missing lextale but exp found
  filter(!Participant.id %in% both$Participant.id) %>% 
  filter(Participant.id %in% exp$participant) # 3 also missing experiment who said they did it 

missing_lextale_and_exp_mascomp = completed_exp %>% # the three in question
  filter(!Participant.id %in% both$Participant.id) %>% 
  filter(!Participant.id %in% exp$participant) # 3 also missing experiment who said they did it 

missing_exp = both %>% 
  filter(!Participant.id %in% exp$participant)


have_current = exp %>% 
  filter(participant %in% both$Participant.id)
unique(have_current$participant)
h = lextale_files %>%
  filter(id %in% both$Participant.id)
unique(h$id)