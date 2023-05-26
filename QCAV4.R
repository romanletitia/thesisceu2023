# Load required packages
library(QCA)
library(SetMethods)
library(readxl)
library(ggplot2)
library(ggpubr)
library(extrafont)

# Load Garamond font
font_import(pattern = "Garamond")

# Read the data from an Excel file into a data frame
data <- data.frame(read_excel("QCA4.xlsx"))

# Convert column names to uppercase
names(data) <- toupper(names(data))

# Define the condition conditions to be included in the analysis
conds <- c("ECC","EF","RS", "RD", "RC", "HPO", "HTM")

# Perform QCA analysis on the data
# Identify the necessary conditions for the outcome condition and non-outcome
QCAfit(data[, 4:10],
       data$OUTCOME,
       necessity = TRUE)

QCAfit(data[, 4:10],
       1- data$OUTCOME,
       necessity = TRUE)

# Create a truth table using the specified conditions
# Set the minimum consistency level to 0.84
# Set the minimum number of cases to 1
# Sort by outcome first, then by condition inclusion
# Include all possible combinations of conditions
TT_y <- truthTable(data = data,
                   outcome  = "OUTCOME",
                   cases=TRUE,
                   conditions = conds,
                   incl.cut = 0.84,
                   n.cut = 1,
                   sort.by = c("OUT", "incl"),
                   complete = TRUE)

# Print the truth table
TT_y

# Minimize the truth table to produce conservative solution
# Return detailed information about the minimization
sol_yc <- minimize(TT_y,
                   details = TRUE)

# Print the conservative solution
sol_yc

# Minimize the truth table to produce the most parsimonious
# Return detailed information about the minimization
sol_yp <- minimize(TT_y,
                   details = TRUE,
                   complete=TRUE,
                   cases=TRUE,
                   include = "?")

# Print the most parsimonious solution
sol_yp

# Minimize the truth table to produce the intermediate solution
# Set the directional expectations of conditions to determined which logical remainders should be included
# Return detailed information about the minimization
sol_yi <- minimize(TT_y,
                   details = TRUE,
                   include = "?",
                   dir.exp = c(1, 1, 0, 1,1,1,1))

# Print the intermediate solution
sol_yi

#Same for non-outcome
TT_ny <- truthTable(data = data,
                    outcome  = "~OUTCOME",
                    conditions = conds,
                    incl.cut = 0.9,
                    n.cut = 2,
                    sort.by = c("OUT", "incl"),
                    complete = TRUE)

TT_ny

# Minimize the truth table to produce the most parsimonious
# Return detailed information about the minimization
sol_np <- minimize(TT_ny,
                   details = TRUE,
                   include = "?")

# Print the most parsimonious solution
sol_np


# Minimize the truth table to produce conservative solution
# Return detailed information about the minimization
sol_nyc <- minimize(TT_ny,
                    details = TRUE)

# Print the conservative solution

sol_nyc


#Typical cases

typ_foc <- smmr(sol_yp,
                outcome ="outcome",
                match = FALSE,
                cases = 2)
typ_foc


#Deviant cases

dcov <- smmr(results = sol_yp,
             outcome = "OUTCOME",
             match = FALSE,
             cases = 4)
dcov

##Plots for casualties

#Ethiopia
# Clean the dataset, aggragate the obs/year

nsce <- read_excel("ucdp-nonstate-221.xlsx")
nsce<-nsce%>%filter(location=="Ethiopia")
nsce<-nsce%>%filter(year==1991|year==1992|year==1993|year==1994|year==1995|year==1996|year==1997|year==1998|year==1999|year==2000|year==2001|year==2002|year==2003|year==2004|year==2005|year==2006)
nsce<-nsce%>%select(-low_fatality_estimate, -high_fatality_estimate)
nsce<-nsce%>%group_by(year)%>%summarise(agg=sum(best_fatality_estimate))


# Customizing the plot with Garamond font
ggplot(nsce, aes(x = year, y = agg)) +
  geom_line(color = "burlywood", size = 1.5, linetype = "solid") +
  geom_point(size = 2, color = "darksalmon") +
  geom_point(size = 5, shape = 24, color = "burlywood") +
  labs(x = "Year", y = "Number of Casualties") +
  ggtitle("Number of Casualties per Year, Ethiopia") +
  scale_x_continuous(breaks = seq(1991, 2006, by = 1)) +
  scale_y_continuous(breaks = seq(25, 1100, by = 50)) +
  theme_pubclean(base_family = "Garamond", base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



#Somalia

# Load
nscs <- read_excel("ucdp-nonstate-221.xlsx")
View(nscs)

# Clean the dataset, aggragate the obs/year

nscs<-nscs%>%filter(location=="Somalia")
nscs<-nscs%>%filter(year==1991|year==1993|year==1994|year==1995|year==1996|year==1997|year==1998|year==1999|year==2000|year==2001|year==2002|year==2003|year==2004|year==2005|year==2006)
nscs<-nscs%>%select(-low_fatality_estimate, -high_fatality_estimate)
nscs<-nscs%>%group_by(year)%>%summarise(agg=sum(best_fatality_estimate))

# Load Garamond font
font_import(pattern = "Garamond")

# Customizing the plot with Garamond font
ggplot(nscs, aes(x = year, y = agg)) +
  geom_line(color = "black", size = 1.5, linetype = "solid") +
  geom_point(size = 2, color = "red3") +
  geom_point(size = 5, shape = 24, color = "black") +
  geom_rect(aes(xmin = 2001, xmax = 2004, ymin = -Inf, ymax = Inf), fill = "gray89", alpha = 0.07) +
  geom_rect(aes(xmin = 2003.75, xmax = 2004.5, ymin = -Inf, ymax = Inf), fill = "red3", alpha = 0.07) +
  geom_rect(aes(xmin = 2004.5, xmax = 2006, ymin = -Inf, ymax = Inf), fill = "grey59", alpha = 0.07) +
  labs(x = "Year", y = "Number of Casualties") +
  ggtitle("Number of Casualties per Year, Somalia") +
  scale_x_continuous(breaks = seq(1991, 2006, by = 1)) +
  scale_y_continuous(breaks = seq(25, 700, by = 25)) +
  theme_pubclean(base_family = "Garamond", base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(x = 2004, y = 625, label = "Spike in violence", color = "black", size = 5, fontface = "bold") +
  geom_vline(xintercept = 2001, linetype = "dashed", color = "black", size = 1.2)+
  geom_vline(xintercept = 2006, linetype = "dashed", color = "red4", size = 1.2)


#MAP SOMALIA

library(sf)
library(ggpubr)

somalia<- st_read("som_admbnda_adm2_ocha_20230308.shp")

somalia <- somalia %>% 
  group_by(ADM1_EN) %>% 
  mutate(centroid = st_centroid(geometry))

ggplot(data = somalia) +
  geom_sf(aes(fill = case_when(
    ADM1_EN %in% c("Sanaag", "Sool") ~ "Drought",
    ADM1_EN == "Nugaal" ~ "Tsunami",
    TRUE ~ "Normal"), geometry = geometry), color = "black") +
  theme_pubr(base_size = 12, base_family = "EBGaramond") +
  theme(legend.position = "right", legend.direction = "horizontal") +
  geom_point(aes(x = 46.4153, y = 8.9541, color = "Violent outbreaks"), shape = 17, size = 3) +
  geom_point(aes(x = 46.4153, y = 8.9541, color = "Violent outbreaks"), shape = 1, size = 7) +
  geom_point(aes(x = 46.4153, y = 8.9541, color = "Violent outbreaks"), shape = 1, size = 11) +
  geom_point(aes(x = 46.4153, y = 8.9541, color = "Violent outbreaks"), shape = 1, size = 14) +
  geom_point(aes(x = 49.8151, y = 7.9808, color = "Violent outbreaks"), shape = 17, size=3) +
  geom_point(aes(x = 49.8151, y = 7.9808, color = "Violent outbreaks"), shape = 1, size=7) +
  geom_point(aes(x = 49.8151, y = 7.9808, color = "Violent outbreaks"), shape = 1, size=11) +
  geom_point(aes(x = 49.8151, y = 7.9808, color = "Violent outbreaks"), shape = 1, size=14) +
  geom_point(aes(x = 47.2167, y = 9.9279, color = "Violent outbreaks"), shape = 17, size = 3) +
  geom_point(aes(x = 47.2167, y = 9.9279, color = "Violent outbreaks"), shape = 1, size = 7) +
  geom_point(aes(x = 47.2167, y = 9.9279, color = "Violent outbreaks"), shape = 1, size = 11) +
  geom_point(aes(x = 47.2167, y = 9.9279, color = "Violent outbreaks"), shape = 1, size = 14) +
  scale_fill_manual(values = c("Drought" = "darkgoldenrod1", "Normal" = "cornsilk", "Tsunami" = "cornflowerblue"), 
                    name = "Extreme weather condition:", 
                    labels = c("Drought", "Normal", "Tsunami")) +
  scale_color_manual(values = c("Violent outbreaks" = "red4"), name = " Events", labels = c("Violent outbreaks. Total number of casualties in 2005 = 600 (UCDP, 2023)"))+
  theme(axis.title = element_blank()) +
  geom_text(aes(x = st_coordinates(centroid)[, 1],
                y = st_coordinates(centroid)[, 2],
                label = ADM1_EN),
            check_overlap = TRUE, fontface = "bold")  # make the text bold






