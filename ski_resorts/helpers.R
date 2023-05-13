# Helper file for ui.R and server.R

# path to data ####
PATH_RESORTS="input/mg_resorts_gs.csv"

# Selection boxes for search refinement ####

THREE_SELECT = list("N/A" = -1,
                    "Yes" = 1,
                    "No" = 0)


# The names for slopes difficulties ########
MENU_SLOPES = c("Beginner",
                "Intermediate",
                "Difficult")

# The name of the menu indicating that nothing specific matters ####
MENU_ANY = "Any"

# read the ski resorts data
ski_data <- read_csv(PATH_RESORTS)