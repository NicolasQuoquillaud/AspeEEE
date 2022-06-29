#' Recodage des codes espèces et exclusion de taxons
#'
#' Par exemple les carpes cuir, miroir, etc. sont regroupées sous un unique code CCX.
#' Idem pour les vandoises en VAX.
# 'Dans l'ouest Finistère, il n'y a que de l'épinoche => recodage de l'épinochette sur cette zone.
#'
#' @param df Dataframe standardisé contenant les données.
#' @param sp_to_remove Vecteur texte contenant les codes à trois lettres des taxons à
#'     supprimer.
#'
#' @return Le dataframe mis à jour.
#' @export
#'
#' @importFrom stringr str_replace
#'
#' @examples
#' \dontrun{
#' df_propre <- df_brut %>%
#' recode_and_filter_species(sp_to_remove = c("OCL", "ASA", "ASL", "CRC, "PCC", "PFL", "HBG"))
#' }
recode_and_filter_species <- function(df, sp_to_remove = NA) {

  if(!is.na(sp_to_remove))

  {

    df <- df %>%
      filter(!esp_code_alternatif %in% sp_to_remove)
  }


  df <- df %>%
    mutate(esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "BRX", replacement = "BRE"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "CAX", replacement = "CAS"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "CAA", replacement = "CAS"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "CAD", replacement = "CAS"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "CAG", replacement = "CAS"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "CAK", replacement = "CCO"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "CCU", replacement = "CCO"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "CMI", replacement = "CCO"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "GAX", replacement = "GAH"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "GAM", replacement = "GAH"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "GOX", replacement = "GOU"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "GOO", replacement = "GOU"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "VAR", replacement = "VAN"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "BBX", replacement = "BBG"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "VAB", replacement = "VAI"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "VAC", replacement = "VAI"),
           esp_code_alternatif = str_replace(esp_code_alternatif, pattern = "PHX", replacement = "VAI"),
           esp_code_alternatif = ifelse(esp_code_alternatif == "EPT" & x_wgs84 < (-4.1), "EPI", esp_code_alternatif))

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
