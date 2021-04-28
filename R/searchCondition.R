#'Function to construct search conditions to be 'rbind'ed into one dataframe to be passed as conditions argument to any search function
#'
#'@param subject the field you are conditioning on e.g. 'location_identifiers'
#'@param verb the operator you want to use to connect 'subject' and 'object' e.g. includes, between.. A full list of all operators available can be obtained with the call 'getOperators()'
#'@param object the values of the condition as a CHARACTER VECTOR e.g. c('germany', 'france')
#'@return a data.frame row that can be easily 'rbind'ed to other search conditions to be passed in a searchForXXX(search_conditions) call to search for certain entities
#'
#' @author Layla Rohkohl, \email{byehity@gmail.com}
#'
#' @examples
#' searchCondition(subject = "location_identifiers", verb = "includes", object = c("germany", "france"))
#'
#' @import stringr
#' @import dplyr
#' @export
#'
searchCondition <- function(subject, verb, object) {
        # Check that verb is one of the operators
        if (!verb %in% getOperators()$operators) {
                stop(
                        "Verb must be a valid operator. Call getOperators() to view them. Keep in mind that the operator must match the class of the subject."
                )
        }
        # Check that object exists
        if (missing(object)) {
                stop(
                        "An object is missing. Check documentations or use autocomplete to find the correct value(s) for the object part."
                )
        }

        # String check to lower and replace spaces with _ e.g. "Founded on" to "founded_on"
        subject <-
                stringr::str_to_lower(subject) %>% str_replace_all(" ", "_")

        # String check for values
        object <- stringr::str_to_lower(object) %>% str_trim() %>% str_replace_all(" ", "-")

        # Depending on length of object, make a simple row or more complex
        search_row <- data.frame(
                "type" = "predicate",
                "field_id" = subject,
                "operator_id" = verb,
                "values" = data.frame(matrix(
                        nrow = 1,
                        ncol = 1,
                        data = list(as.list(object))
                ))
        )

        # Rename
        colnames(search_row) <-
                c("type", "field_id", "operator_id", "values")
        # Return search_row
        return(search_row)
}