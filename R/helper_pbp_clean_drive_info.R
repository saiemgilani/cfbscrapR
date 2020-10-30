#' Clean Drive Information
#' Cleans CFB (D-I) Drive-By-Drive Data to create `pts_drive` column
#'
#' @param drive_df (\emph{data.frame} required) Drive dataframe pulled from API via the `cfb_drives()` function
#' @details Cleans CFB (D-I) Drive-By-Drive Data to create `pts_drive` column. Requires the following columns be present:
#' \itemize{
#' \item{drive_id}{Returned as `drive_id`}
#' \item{drive_result}{End result of the drive}
#' \item{scoring}{Logical flag for if drive was a scoring drive}
#' \item{game_id}{Unique game identifier}
#' }
#' @return The original `drive_df` with the following columns appended to it:
#' \describe{
#' \item{drive_id}{Returned as `drive_id` from original variable `drive_id`}
#' \item{pts_drive}{End result of the drive}
#' \item{scoring}{Logical flag for if drive was a scoring drive updated}
#' }
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate arrange case_when
#' @export
#'

clean_drive_info <- function(drive_df){
  
  clean_drive = drive_df %>%
    dplyr::mutate(
      pts_drive = dplyr::case_when(
        .data$drive_result == "TD" ~ 7,
        stringr::str_detect(.data$drive_result,"SF") ~ -2,
        .data$drive_result == 'FG GOOD' ~ 3,
        .data$drive_result == 'FG' ~ 3,
        .data$drive_result == 'MISSED FG TD' ~ -7,
        .data$drive_result == 'KICKOFF RETURN TD' ~ -7,
        .data$drive_result == 'END OF HALF TD' ~ 7,
        .data$drive_result == "END OF GAME TD" ~ 7,
        .data$drive_result == 'PUNT RETURN TD' ~ -7,
        .data$drive_result == 'PUNT TD' ~ -7,
        .data$drive_result == 'INT TD' ~ -7,
        .data$drive_result == 'INT RETURN TOUCH' ~ -7,
        .data$drive_result == 'FUMBLE RETURN TD' ~ -7,
        .data$drive_result == 'FUMBLE TD' ~ -7,
        .data$drive_result == 'DOWNS TD' ~ -7,
        stringr::str_detect(.data$drive_result,"TD") ~ 7,
        TRUE ~ 0),
      scoring = ifelse(.data$pts_drive != 0, TRUE, .data$scoring)) %>%
    dplyr::mutate(drive_id = as.numeric(.data$drive_id)) %>%
    dplyr::arrange(.data$game_id, .data$drive_id)
  
  return(clean_drive)
}