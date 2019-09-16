bach <- xml2::read_html("https://imslp.org/wiki/List_of_works_by_Johann_Sebastian_Bach") 
bach <- html_nodes(bach, "table")
bach <- map(bach[2:40], html_table, fill = TRUE)
bach <- map_df(bach, ~mutate_if(.,is.integer, as.character))
bach <- bach %>% mutate(Key = gsub("G mnor", "G minor", Key), Key = as.factor(Key))
bach <- bach %>% filter(Key != "")
bach <- summary(bach$Key) %>% 
  enframe(name = "Key") %>% 
  filter(Key !="") %>% 
  mutate(Key = gsub("\u266d", "b", Key))
bach %>% 
  ggplot(aes(x=Key, y=value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))


mo <- xml2::read_html("https://imslp.org/wiki/List_of_works_by_Wolfgang_Amadeus_Mozart") 
mo <- html_nodes(mo, "table") %>% html_table() %>% .[[1]]
mo2 <- mo %>% mutate(Key = gsub("mnor", "minor", Key),
                    Key = gsub("  ", " ", Key),
                    Key = gsub("\u266d", "b", Key),
                    Key = as.factor(Key),
                    Genre = as.factor(Genre),
                    Forces = as.factor(Forces)
                    )
mo3 <- mo2 %>% filter(Key != "")


mo3 %>% 
  ggplot(aes(x=Key)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45))


mo2 %>% 
  ggplot(aes(x=Key)) +
  geom_bar() +
  coord_polar() +
  theme(axis.text.x = element_text(angle = 45))

#----

musickeys <- c("C major","A minor","G major","E minor","D major","B minor","A major",
               "F♯ minor","E major","Fb major","C♯ minor","Db minor", "B major","Cb major", 
               "G♯ minor","Ab minor", "Gb major","F♯ major", "Eb minor","D♯ minor",  "Db major",
               "C♯ major", "Bb minor","A♯ minor","Ab major","G♯ major","F minor", "Eb major", 
               "C minor", "Bb major", "G minor","F major","D minor")
mo4 <- mo3 %>% mutate(Keys = factor(Key, levels = musickeys, ordered = TRUE))


mo5 <- mo4 %>% 
  mutate(Key2 = 
           #Key factor
           fct_collapse(mo4$Keys,
                        "C major" = c("C major"),  
                        "A minor" = c("A minor"),  
                        "G major" = c("G major"),  
                        "E minor" = c("E minor"),  
                        "D major" = c("D major"),  
                        "B minor" = c("B minor"), 
                        "A major" = c("A major"),  
                        "F♯ minor"  = c("F♯ minor"),
                        "E/Fb major" = c("E major","Fb major"),   
                        "C♯/Db minor"  = c("C♯ minor","Db minor"), 
                        "B/Cb major"   = c("B major","Cb major"), 
                        "G♯/Ab minor" = c("G♯ minor","Ab minor"), 
                        "Gb/F♯ major" = c("Gb major","F♯ major"), 
                        "Eb/D♯ minor" = c("Eb minor","D♯ minor"),  
                        "Db/C♯ major" = c("Db major","C♯ major"), 
                        "Bb/A♯ minor" = c("Bb minor","A♯ minor"),  
                        "Ab/G♯ major" = c("Ab major","G♯ major"),  
                        "F minor" = c("F minor"),
                        "Eb major" = c("Eb major"),
                        "C minor" = c("C minor"),
                        "Bb major" = c("Bb major"),
                        "G minor" = c("G minor"),
                        "F major" = c("F major"),
                        "D minor" = c("D minor")
           )
  )

fct_count(mo5$Key2) %>% 
  ggplot(aes(x=f, y=n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  coord_polar()



mo5 %>% 
  ggplot(aes(x=Key2)) +
  geom_bar(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45))

#----

musickeys <- c("C major","A minor","G major","E minor","D major","B minor","A major",
               "F♯ minor","E major","Fb major","C♯ minor","Db minor", "B major","Cb major", 
               "G♯ minor","Ab minor", "Gb major","F♯ major", "Eb minor","D♯ minor",  "Db major",
               "C♯ major", "Bb minor","A♯ minor","Ab major","G♯ major","F minor", "Eb major", 
               "C minor", "Bb major", "G minor","F major","D minor")
bachagain <- bach %>% mutate(Keys = factor(Key, levels = musickeys, ordered = TRUE))


bachagain <- bachagain %>% 
  mutate(Key2 = 
           #Key factor
           fct_collapse(bachagain$Keys,
                        "C maj" = c("C major"),  
                        "A min" = c("A minor"),  
                        "G maj" = c("G major"),  
                        "E min" = c("E minor"),  
                        "D maj" = c("D major"),  
                        "B min"    = c("B minor"), 
                        "A maj"    = c("A major"),  
                        "F♯ min"   = c("F♯ minor"),
                        "E/Fb maj" = c("E major","Fb major"),   
                        "C♯/Db min"  = c("C♯ minor","Db minor"), 
                        "B/Cb maj"   = c("B major","Cb major"), 
                        "G♯/Ab min" = c("G♯ minor","Ab minor"), 
                        "Gb/F♯ maj" = c("Gb major","F♯ major"), 
                        "Eb/D♯ min" = c("Eb minor","D♯ minor"),  
                        "Db/C♯ maj" = c("Db major","C♯ major"), 
                        "Bb/A♯ min" = c("Bb minor","A♯ minor"),  
                        "Ab/G♯ maj" = c("Ab major","G♯ major"),  
                        "F min" = c("F minor"),
                        "Eb maj" = c("Eb major"),
                        "C min" = c("C minor"),
                        "Bb maj" = c("Bb major"),
                        "G min" = c("G minor"),
                        "F maj" = c("F major"),
                        "D min" = c("D minor")
           )
  )

bachagain %>% 
  ggplot(aes(x=Key2, y=value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  coord_polar()

mo5 %>% 
  ggplot(aes(x=Key2)) +
  geom_bar(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45))

#----

inst <- xml2::read_html("https://imslp.org/wiki/IMSLP:Abbreviations_for_Instruments") %>% 
html_nodes("table") %>% 
  html_table() %>% 
  .[[1]]

summary(mo2$Forces) %>% 
  names() %>% 
  strsplit(" ") %>% 
  flatten() %>% 
  map_chr(1) %>% 
  unique()

inst$English

##Motzart
"C major"  
"A minor"  
"G major"  
"E minor"  
"D major"  
"B minor" 
"A major"  
#"F♯ minor"
"E major"  
#"C♯ minor"
#"B major"
#"G♯ minor"
#"Gb major"/"F♯ major"
#"Eb minor" 
#"Db major" 
#"Bb minor" 
"Ab major" 
"F minor"  
"Eb major" 
"C minor"  
"Bb major" 
"G minor"  
"F major"  
"D minor"  

##Bach
"C major"  
"A minor"  
"G major"  
"E minor"  
"D major"  
"B minor" 
"A major"  
"F♯ minor"
"E major"  
"C♯ minor"
"B major"
"G♯ minor"
#"Gb major"/"F♯ major" *
"Eb minor" 
#"Db major"/"C♯ major" *
"Bb minor" 
"Ab major" 
"F minor"
"Eb major"
"C minor"
"Bb major"
"G minor"
"F major"
"D minor"

#Key factor
fct_collapse(mo3$Key,
"C major" = c("C major"),  
"A minor" = c("A minor"),  
"G major" = c("G major"),  
"E minor" = c("E minor"),  
"D major" = c("D major"),  
"B minor" = c("B minor"), 
"A major" = c("A major"),  
"F♯ minor"  = c("F♯ minor"),
"E/Fb major" = c("E major","Fb major"),   
"C♯/Db minor"  = c("C♯ minor","Db minor"), 
"B/Cb major"   = c("B major","Cb major"), 
"G♯/Ab minor" = c("G♯ minor","Ab minor"), 
"Gb/F♯ major" = c("Gb major","F♯ major"), 
"Eb/D♯ minor" = c("Eb minor","D♯ minor"),  
"Db/C♯ major" = c("Db major","C♯ major"), 
"Bb/A♯ minor" = c("Bb minor"/"A♯ minor"),  
"Ab/G♯ major" = c("Ab major"/"G♯ major"),  
"F minor" = c("F minor"),
"Eb major" = c("Eb major"),
"C minor" = c("C minor"),
"Bb major" = c("Bb major"),
"G minor" = c("G minor"),
"F major" = c("F major"),
"D minor" = c("D minor")
)

fct_collapse(mo3$Key,
             `C major`= c("C major"),  
             `A minor`= c("A minor"),  
             `G major`= c("G major"),  
             `E minor`= c("E minor"),  
             `D major`= c("D major"),  
             `B minor`= c("B minor"), 
             `A major`= c("A major"),  
             `F♯ minor` = c("F♯ minor"),
             `E/Fb major`= c("E major","Fb major"),   
             `C♯/Db minor` = c("C♯ minor","Db minor"), 
             `B/Cb major`  = c("B major","Cb major"), 
             `G♯/Ab minor`= c("G♯ minor","Ab minor"), 
             `Gb/F♯ major`= c("Gb major","F♯ major"), 
             `Eb/D♯ minor`= c("Eb minor","D♯ minor"),  
             `Db/C♯ major`= c("Db major","C♯ major"), 
             `Bb/A♯ minor`= c("Bb minor"/"A♯ minor"),  
             `Ab/G♯ major`= c("Ab major"/"G♯ major"),  
             `F minor`= c("F minor"),
             `Eb major`= c("Eb major"),
             `C minor`= c("C minor"),
             `Bb major`= c("Bb major"),
             `G minor`= c("G minor"),
             `F major`= c("F major"),
             `D minor`= c("D minor")
)



