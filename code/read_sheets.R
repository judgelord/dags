# this script reads edges and nodes from a google sheet
# library(devtools)
# install_github("judgelord/netlit")


library(netlit)
library(tidyverse)
library(googlesheets4)

gs4_auth(email = "devin.jl@gmail.com")

# drop NA
edges <- read_sheet("1C5frNDGSIwaR-a6QYZaWM-f-vSCVYSJKcvA-bILvEsg",
                    sheet = "edges") |>
  drop_na(from)

# helper function to format labels
clean <- function(x){
  x |> str_replace_all(" ", "\n") |>
    str_replace_all("\n([A-z]{2})\n", " \\1\n") |>
    str_to_sentence()
}

clean("officials interfere in elections")


edges |>
  mutate(from = from |> clean(),
         to = to |> clean(),
         # colorblind friendly
         color = str_replace(color, "blue", "#3B99B1"),
         color = str_replace(color, "red", "#F5191C")) |>
  write_csv("edges.csv")

node_attributes <- read_sheet("1C5frNDGSIwaR-a6QYZaWM-f-vSCVYSJKcvA-bILvEsg",
                              sheet = "nodes") |>

  drop_na(node)

node_attributes |>
  mutate(node = node |> clean(),
         # colorblind friendly
         color = str_replace(color, "blue", "#3B99B1"),
         color = str_replace(color, "red", "#F5191C")) |>
  write_csv("node_attributes.csv")



#################### LOAD DATA ########################

# load data for example dags
edges <- read_csv("edges.csv") # https://github.com/judgelord/dags/blob/main/edges.csv
node_attributes <- read_csv("node_attributes.csv") # https://github.com/judgelord/dags/blob/main/node_attributes.csv

# a plot function based on netlit's review() function
netlit_plot <- function(edges){
  # create a graph with netlit's review() function
  netlit::review(edges,
                 edge_attributes = names(edges),
                 node_attributes = node_attributes
                 ) |>
    # pluck out the graph object
    pluck("graph") |>
    # plot using the default igraph plot function
    plot( margin=0)
}
# (for fancier plots, see see judgelord.github.io/netlit/articles)

par(mar=c(0,0,0,0))

filter(edges, case == "forestry") |>
  netlit_plot()

filter(edges, cites == "Judge-Lord 2021")  |>
  netlit_plot()


filter(edges, cites == "Balla et al. 2020") |>
  netlit_plot()

filter(edges, cites == "river-state") |>
  netlit_plot()

filter(edges, cites == "river-city")  |>
  netlit_plot()

filter(edges, case == "landholding")  |>
  netlit_plot()

filter(edges, case == "landholing-extended")  |>
  netlit_plot()


filter(edges, case == "democracy")  |>
  filter(cites != "Wedeen 2007; Habermas 1989") |>
  netlit_plot()

