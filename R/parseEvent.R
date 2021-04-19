#' Function to parse Event Entity from JSON list to Data.frame
#'
#'@param data Returned fields card response in json list form for the entity
#'@return  a data.frame of parsed output
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' parseEvent(data$cards$fields)
#'
#'@import stringr
#'
# Parse Event Entity
parseEvent <- function(data) {
  # Identifier parsing
  ids <- parseIdentifier(data[["identifier"]], "identifier")
  organizer_identifiers <- parseIdentifier(data[["organizer_identifiers"]], "organizer_identifiers")

  # Simple parsing
  short_description <- simpleParse(data[["short_description"]])
  description <- simpleParse(data[["description"]])
  venue_name <- simpleParse(data[["venue_name"]])
  starts_on <- simpleParse(data[["starts_on"]])
  ends_on <- simpleParse(data[["ends_on"]])
  num_sponsors <- simpleParse(data[["num_sponsors"]])
  event_type <- str_replace_all(simpleParse(data[["event_type"]]),
                                pattern = c(
                                  "class" = "Class",
                                  "competition" = "Competition",
                                  "conference" = "Conference",
                                  "demo_day" = "Demo Day",
                                  "expo" = "Expo",
                                  "festival" = "Festival",
                                  "hackathon" = "Hackathon",
                                  "meetup" = "Meetup",
                                  "networking" = "Networking",
                                  "other" = "Other",
                                  "seminar" = "Seminar",
                                  "virtual" = "Virtual",
                                  "webinar" = "Webinar"
                                ))
  num_speakers <- simpleParse(data[["num_speakers"]])
  num_contestants <- simpleParse(data[["num_contestants"]])
  num_organizers <- simpleParse(data[["num_organizers"]])
  num_relationships <- simpleParse(data[["num_relationships"]])
  rank_event <- simpleParse(data[["rank_event"]])
  rank <- simpleParse(data[["rank"]])
  num_exhibitors <- simpleParse(data[["num_exhibitors"]])
  image_id <- simpleParse(data[["image_id"]])
  image_url <- simpleParse(data[["image_url"]])
  created_at <- simpleParse(data[["created_at"]])
  updated_at <- simpleParse(data[["updated_at"]])

  # List parsing
  location_identifiers <- listParse(data[["location_identifiers"]])
  location_group_identifiers <- listParse(data[["location_group_identifiers"]])
  categories <- listParse(data[["categories"]])
  category_groups <- listParse(data[["category_groups"]])
  registration_url <- listParse(data[["registration_url"]])
  event_url <- listParse(data[["event_url"]])

  # Put into one dateframe
  df <- data.frame(cbind(  ids ,
                           short_description ,
                           description ,
                           venue_name,
                           starts_on ,
                           ends_on ,
                           location_identifiers,
                           location_group_identifiers,
                           event_type ,
                           rank,
                           rank_event ,
                           num_speakers ,
                           num_sponsors ,
                           num_exhibitors ,
                           num_contestants ,
                           num_relationships,
                           num_organizers,
                           organizer_identifiers,
                           image_id,
                           image_url ,
                           created_at ,
                           updated_at, # Simple parsing done
                           categories ,
                           category_groups ,
                           event_url,
                           registration_url

  ))

  # Adjust classes
  # as numeric
  df$rank <- as.numeric(df$rank)
  df$rank_event <- as.numeric(df$rank_event)
  df$num_speakers <- as.numeric(df$num_speakers)
  df$num_sponsors <- as.numeric(df$num_sponsors)
  df$num_exhibitors <- as.numeric(df$num_exhibitors)
  df$num_contestants <- as.numeric(df$num_contestants)
  df$num_organizers <- as.numeric(df$num_organizers)
  df$num_relationships <- as.numeric(df$num_relationships)

  # as date
  df$starts_on <- as.Date(df$starts_on)
  df$ends_on <- as.Date(df$ends_on)
  df$created_at <- as.Date(df$created_at)
  df$updated_at <- as.Date(df$updated_at)

  # Return dataframe
  return(df)
}
