library(tidyverse)
library(here)



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

exp <- fs::dir_ls(here("data", "main_exp"),
                  regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) 

missing_lextale = completed_exp %>% # missing lextale but exp found
  filter(!Participant.id %in% both$Participant.id) %>% 
  filter(Participant.id %in% exp$participant) # 3 also missing experiment who said they did it 


ppts_remove = c("5bbbdca68f3bd70001e713ef",
                "6789000b08f2703ba0b959b1",
                "63b87d2c9edd47ad702413a1",
                "5ed2edb5b7f800107d47c777",
                "6046ac1161b60d0fc94b0000",
                "6789000b08f2703ba0b959b1",
                "603a80b01bb75f608c3dbb0d",
                "627bfd52e3e0bfd01cd0cad5",
                "5a06ebf2c259f300017656ee")


missing_lextale$Participant.id %in% ppts_remove


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

have_current %>% write.csv(here("data", "tidy", "complete_ppts.csv"))


### correct proper files 

m1 = read.csv(here("data","find_lextales", "extracted_files", "m_1.csv"))

m1$participant = c("60411127dba45b0008bf6bc4")

m1 %>% write.csv(here("data", "main_exp", "60411127dba45b0008bf6bc4.csv"))


m2 = read.csv(here("data","find_lextales", "extracted_files", "m_2.csv"))

m2$participant = c("6156c03beb47b00976ab809f")

m2 %>% write.csv(here("data", "main_exp", "6156c03beb47b00976ab809f.csv"))


m3 = read.csv(here("data","find_lextales", "extracted_files", "m_3.csv"))

m3$participant = c("63f6216fd56701af4b7188f4")

m3 %>% write.csv(here("data", "main_exp", "63f6216fd56701af4b7188f4.csv"))


m4 = read.csv(here("data","find_lextales", "extracted_files", "m_4.csv"))

m4$participant = c("5d0f89123382180001624f48")

m4 %>% write.csv(here("data", "main_exp", "5d0f89123382180001624f48.csv"))


m5 = read.csv(here("data","find_lextales", "extracted_files", "m_5.csv"))

m5$participant = c("5a0acda16e1ea9000124e675")

m5 %>% write.csv(here("data", "main_exp", "5a0acda16e1ea9000124e675.csv"))


m6 = read.csv(here("data","find_lextales", "extracted_files", "m_6.csv"))

m6$participant = c("5e5b7eee4989f13610bb461b")

m6 %>% write.csv(here("data", "main_exp", "5e5b7eee4989f13610bb461b.csv"))


m7 = read.csv(here("data","find_lextales", "extracted_files", "m_7.csv"))

m7$participant = c("63deb91fed937fd1bd573df7")

m7 %>% write.csv(here("data", "main_exp", "63deb91fed937fd1bd573df7.csv"))

m8 = read.csv(here("data","find_lextales", "extracted_files", "m_8.csv"))

m8$participant = c("5e09a01ba2c69625cd2fefb8")

m8 %>% write.csv(here("data", "main_exp", "5e09a01ba2c69625cd2fefb8.csv"))

m9 = read.csv(here("data","find_lextales", "extracted_files", "m_9.csv"))

m9$participant = c("5ec7ce2dd402d6256c260e0d")

m9 %>% write.csv(here("data", "main_exp", "5ec7ce2dd402d6256c260e0d.csv"))

