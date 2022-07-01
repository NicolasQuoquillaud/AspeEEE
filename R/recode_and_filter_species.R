#' Recodage des codes espèces et exclusion de taxons
#'
#' Par exemple les carpes cuir, miroir, etc. sont regroupées sous un unique code CCO.
#'
#' @param df Dataframe standardisé contenant les données.
#' @param sp_to_remove Vecteur texte contenant les codes à trois lettres des taxons à
#'     supprimer.
#'
#' @return Le dataframe mis à jour.
#' @export
#'
#' @importFrom dplyr mutate filter pull case_when
#'
#' @examples
#' \dontrun{
#' df_propre <- df_brut %>%
#' recode_and_filter_species(sp_to_remove = c("OCL", "ASA", "ASL", "CRC, "PCC", "PFL", "HBG"))
#' }
recode_and_filter_species <- function(df,
                                      sp_to_remove = NA) {

  if(!is.na(sp_to_remove))

  {

    df <- df %>%
      filter(!esp_code_alternatif %in% sp_to_remove)
  }


  df <- df %>%
    mutate(
      esp_code_alternatif = case_when(
        esp_code_alternatif == "BRX" ~ "BRE",
           esp_code_alternatif == "CAX" ~ "CAS",
           esp_code_alternatif == "CAA" ~ "CAS",
           esp_code_alternatif == "CAD" ~ "CAS",
           esp_code_alternatif == "CAG" ~ "CAS",
           esp_code_alternatif == "CAK" ~ "CCO",
           esp_code_alternatif == "CCU" ~ "CCO",
           esp_code_alternatif == "CMI" ~ "CCO",
           esp_code_alternatif == "GAX" ~ "GAH",
           esp_code_alternatif == "GAM" ~ "GAH",
           esp_code_alternatif == "GOX" ~ "GOU",
           esp_code_alternatif == "GOO" ~ "GOU",
           esp_code_alternatif == "VAR" ~ "VAN",
           esp_code_alternatif == "BBX" ~ "BBG",
           esp_code_alternatif == "VAB" ~ "VAI",
           esp_code_alternatif == "VAC" ~ "VAI",
           esp_code_alternatif == "PHX" ~ "VAI",
           TRUE ~ esp_code_alternatif
        )
      )

  # Permet de sélectionner les codes espèces dans le fichier "passerelle_taxo"
  # disponible dans le package aspe. On ne sélectionne que les codes "valides",
  # c'est à dire les codes de 3 lettres.
  code_valide <- passerelle_taxo %>%
    filter(nchar(esp_code_alternatif) == 3) %>%
    pull(esp_code_alternatif)

  # Filtrer les espèces valides
  df %>%
    filter(esp_code_alternatif %in% code_valide)

}
