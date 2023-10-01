# this script reads edges and nodes from a google sheet
library(devtools)
install_github("judgelord/netlit")


library(netlit)
library(tidyverse)
library(googlesheets4)

gs4_auth(email = "devin.jl@gmail.com")

edges <- read_sheet("1C5frNDGSIwaR-a6QYZaWM-f-vSCVYSJKcvA-bILvEsg", sheet = "edges") |>
  drop_na(from)

edges |>
  mutate(from = from |> str_replace_all(" ", "\n"),
         to = to |> str_replace_all(" ", "\n"),
         # colorblind friendly
         color = str_replace(color, "blue", "#3B99B1"),
         color = str_replace(color, "red", "#F5191C")) |>
  write_csv("edges.csv")

node_attributes <- read_sheet("1C5frNDGSIwaR-a6QYZaWM-f-vSCVYSJKcvA-bILvEsg", sheet = "nodes") |>
  drop_na(node)

node_attributes |>
  mutate(node = node |> str_replace_all(" ", "\n")) |>
  write_csv("node_attributes.csv")



#################### LOAD DATA ########################

# load data
edges <- read_csv("edges.csv")
node_attributes <- read_csv("node_attributes.csv")

# a plot function based on netlit's review() function
netlit_plot <- function(edges){
  netlit::review(edges, edge_attributes = "color",
                 node_attributes = node_attributes)|>
    # pluck out the graph object
    pluck("graph") |>
    # plot using the default igraph plot function
    plot()

}
# (for fancier plots, see see judgelord.github.io/netlit/articles)


filter(edges, cites == "Judge-Lord 2021")  |>
  netlit_plot()


filter(edges, cites == "Balla et al. 2020") |>
  netlit_plot()
