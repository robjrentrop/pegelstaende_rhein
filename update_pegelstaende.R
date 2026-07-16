# =============================================================================
# Pegelstände im breiten Format für Datawrapper
# =============================================================================
# Holt die letzten `TAGE` Tage für alle vier Pegel und schreibt sie als EINE
# CSV im breiten Format (eine Zeile pro Zeitpunkt, eine Spalte pro Pegel) -
# genau das Format, das Datawrapper-Liniendiagramme erwarten.
#
# Wird von der GitHub Action (.github/workflows/update-pegelstaende.yml)
# automatisch alle 30 Minuten ausgeführt. Das Ergebnis (pegelstaende_datawrapper.csv)
# wird per "Externe Daten verlinken" in Datawrapper eingebunden - die
# Roh-URL bei GitHub ist die "dynamische URL":
#
#   https://raw.githubusercontent.com/<username>/<repo>/main/pegelstaende_datawrapper.csv
#
# Benötigte Pakete: jsonlite, dplyr, tidyr
# =============================================================================

library(jsonlite)
library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------
# Konfiguration
# -----------------------------------------------------------------------
stationen <- c(
  "Mannheim"         = "57090802-c51a-4d09-8340-b4453cd0e1f5",
  "Koeln"            = "a6ee8177-107b-47dd-bcfd-30960ccc6e9c",
  "Duisburg-Ruhrort" = "c0f51e35-d0e8-4318-afaf-c5fcbc29f4c1",
  "Kaub"             = "1d26e504-7f9e-480a-b52c-5932be6549ab"
)

api_base <- "https://www.pegelonline.wsv.de/webservices/rest-api/v2"
TAGE     <- 14   # Zeitraum, der in der Datawrapper-Grafik gezeigt wird
AUFLOESUNG_STUNDEN <- TRUE  # TRUE = auf Stundenmittel verdichten (kleinere, sauberere Datei)

ausgabe_pfad <- "pegelstaende_datawrapper.csv"

# -----------------------------------------------------------------------
hole_messwerte <- function(uuid, tage, versuche = 3, wartezeit_sek = 5) {
  url <- sprintf("%s/stations/%s/W/measurements.json?start=P%dD", api_base, uuid, tage)

  for (versuch in seq_len(versuche)) {
    ergebnis <- tryCatch(
      list(erfolg = TRUE, daten = fromJSON(url, simplifyVector = TRUE)),
      error = function(e) list(erfolg = FALSE, fehler = conditionMessage(e))
    )

    if (isTRUE(ergebnis$erfolg) && length(ergebnis$daten) > 0 && nrow(ergebnis$daten) > 0) {
      return(data.frame(
        timestamp = as.POSIXct(ergebnis$daten$timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        wert_cm   = as.numeric(ergebnis$daten$value)
      ))
    }

    grund <- if (isTRUE(ergebnis$erfolg)) "leere Antwort" else ergebnis$fehler
    message(sprintf("  Versuch %d/%d für %s fehlgeschlagen (%s)", versuch, versuche, uuid, grund))
    if (versuch < versuche) Sys.sleep(wartezeit_sek)
  }

  data.frame(timestamp = as.POSIXct(character()), wert_cm = numeric())
}

# -----------------------------------------------------------------------
alle <- lapply(names(stationen), function(name) {
  df <- hole_messwerte(stationen[[name]], TAGE)
  if (nrow(df) == 0) return(NULL)
  df$pegel <- name
  df
})
alle <- bind_rows(alle)

if (nrow(alle) == 0) {
  message("Keine Daten von der API erhalten (auch nach Wiederholungsversuchen). ",
          "Breche ab, ohne die bestehende CSV zu überschreiben. ",
          "Das ist vermutlich ein vorübergehender Aussetzer bei PEGELONLINE - der nächste Lauf sollte wieder funktionieren.")
  quit(status = 0)  # bewusst KEIN Fehler-Exit-Code, damit GitHub Actions das nicht als roten Fehlschlag meldet
}

if (AUFLOESUNG_STUNDEN) {
  alle <- alle %>%
    mutate(timestamp = as.POSIXct(format(timestamp, "%Y-%m-%d %H:00:00"), tz = "UTC")) %>%
    group_by(pegel, timestamp) %>%
    summarise(wert_cm = mean(wert_cm, na.rm = TRUE), .groups = "drop")
}

breit <- alle %>%
  pivot_wider(names_from = pegel, values_from = wert_cm) %>%
  arrange(timestamp) %>%
  rename(Datum = timestamp)

write.csv(breit, ausgabe_pfad, row.names = FALSE)
message(sprintf("Geschrieben: %s (%d Zeilen, Stand %s)", ausgabe_pfad, nrow(breit),
                 format(Sys.time(), "%d.%m.%Y %H:%M:%S")))
