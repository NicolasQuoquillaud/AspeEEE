#' Modéliser des variables en fonction de l'année et de la présence du spirlin
#'
#' Permet de caler une régression multiple de chaque variable spécifiée en fonction de l'année
#'     (variable annee) et de la présence du spirlin (variable spi codée 0/1). Les sorties des modèles sont
#'     synthétisées et mises en forme dans un tableau.
#'
#' @param model_data Dataframe contenant les données. Il contient une colonne par variable à modéliser,
#'     une colonne annee et une colonne spi.
#' @param metriques Vecteur caractères. Nom des colonnes contenant les variables à modéliser.
#' @param interaction Booléen. L'interaction annee x spi doit-elle être incluse en variable
#'     explicative ? Par défaut interaction = TRUE.
#'
#' @return Un dataframe résumant les modèles.
#' @export
#'
#' @importFrom stats lm as.formula cooks.distance
#' @importFrom dplyr filter mutate case_when select rename
#' @importFrom ggplot2 ggplot geom_histogram geom_density geom_vline ggtitle aes
#' @importFrom purrr map reduce
#' @importFrom tibble rownames_to_column
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # avec terme d'interaction
#' resultat <- lm_annee_spi(mod_data, metriques = mes_metriques)
#'
#' # sans terme d'interaction
#' lm_annee_spi(mod_data, metriques = mes_metriques, interaction = FALSE)
#' }
#'
lm_annee_spi <-
  function(model_data, metriques, interaction = TRUE)

  {
    # -------------- Fonction pour une métrique ------------------------- #
    lm_1_var <- function(model_data, metrique, interaction) {
      # construction de la formule en fonction de la variable dépendante
      if (interaction) {
        fm <- as.formula(paste0(metrique,
                                " ~ pop_id + annee * spi"))
      } else {
        fm <- as.formula(paste0(metrique,
                                " ~ pop_id + annee + spi"))
      }

      # on cale le modèle
      mod <- lm(formula = fm,
                data = model_data)

      dist_cook <- cooks.distance(mod)
      seuil_cook <- 4 / nrow(model_data)
      model_data <- model_data %>%
        cbind(dist_cook) %>% # ajout de la colonne avec les distances
        filter(dist_cook < seuil_cook) # suppression des observations avec distance > 4/N

      mod <- lm(formula = fm,
                data = model_data)

      # récupération de ce qui ous intéresse dans les résultats et mise en forme
      if (interaction) {
        resultat <-
          summary(mod)$coefficients[c("(Intercept)",
                                      "annee",
                                      "spiPrésence",
                                      "annee:spiPrésence"), ]
      } else {
        resultat <-
          summary(mod)$coefficients[c("(Intercept)", "annee", "spiPrésence"), ]
      }

      resultat <- resultat %>%
        as.data.frame() %>%
        select(coef = Estimate,
               pval = `Pr(>|t|)`) %>%
        mutate(
          sig = case_when(
            pval > 0.05 ~ "NS",
            pval <= 0.05 & pval > 0.01 ~ "*",
            pval <= 0.01 & pval > 0.001 ~ "**",
            TRUE ~ "***"
          )
        ) %>%
        mutate(text = paste0(round(coef, 3),
                             " (",
                             sig,
                             ")")) %>%
        select(text) %>%
        t()

      # on nomme la ligne d'après le nom de la métrique
      row.names(resultat) <- metrique
      tableau <- data.frame(residus = mod$residuals)

      # sortie
      ggplot(data = tableau, aes(x = residus)) + geom_histogram(aes(y = ..density..), alpha = 0.5) +
        geom_density(alpha = 0.5) +
        geom_vline(
          aes(xintercept = mean(residus, na.rm = T)),
          color = "red",
          linetype = "dashed",
          size = 1
        ) +
        ggtitle("Histogramme du logarythme de IPR")
      resultat
    }
    # -------------- Fin fonction pour une métrique ------------------------- #

    # application à un ensemble de métriques, assemblage et mise en forme
    map(
      .x = metriques,
      .f = lm_1_var,
      model_data = model_data,
      interaction = interaction
    ) %>%
      reduce(rbind) %>% # assemblage, une ligne par modèle
      as.data.frame() %>%
      rownames_to_column() %>%
      rename(Intercept = `(Intercept)`,
             `Métrique` = rowname,
             `Année` = annee)

  }

