# metaSABSEB

Repository erstellt am: 07.10.2025

Release v1.0.0 am: 11.11.2026

## Kontakt

- Christina Arn, Bildungsplanung Kanton Zürich: <christina.arn@bi.zh.ch>
- Marco Schuppisser, ZEM CES: <marco.schuppisser@zemces.ch>

## Kurzbeschreibung

Das R-Package [metaSABSEB](https://github.com/bildungsplanungZH/metaSABSEB) enthält **Metadaten** zum Qualitätsmonitoring Sek II. Es wurde durch die [Bildungsplanung des Kantons Zürich](https://www.zh.ch/de/bildungsdirektion/generalsekretariat-der-bildungsdirektion/bildungsplanung.html) in Zusammenarbeit mit dem
[Schweizerischen Zentrum für die Mittelschule und für Schulevaluation auf Sekundarstufe ||](https://www.zemces.ch/de) (ZEM CES) erarbeitet. 

Konkret geht es um Informationen zu folgenden Befragungen:

- Standardisierte Abschlussklassenbefragung ([SAB](https://www.zemces.ch/de/evaluationen-and-befragungen/befragungen/standardisierte-abschlussklassenbefragung-sab))
- Standardisierte Ehemaligenbefragung ([SEB](https://www.zemces.ch/de/evaluationen-and-befragungen/befragungen/standardisierte-ehemaligenbefragung-seb))

Die Metadaten beschreiben Variablen und Werteausprägungen und liefern wichtige Hintergrundinformationen zu den Daten.
Die gesammelten Metadaten beschränken sich auf die gesamtschweizerisch erhobenen Merkmale sowie auf einzelne Wahlmodule.

Darüber hinaus enthält das Package **Funktionen**, zum Abrufen, Anpassen und Ergänzen von Metadaten.

## Installation

Das R-Package kann direkt in R mit folgendem Befehl installiert werden:
```
devtools::install_github("bildungsplanungZH/metaSABSEB", build_vignettes = TRUE)
```

## Übersicht der Funktionen

* `get_meta()`: Zum Abrufen von Metadaten
* `change_meta()`: Zum Ändern von einzelnen Metadatenfeldern von Variablen, die bereits erfasst wurden
* `add_meta()`: Zum Hinzufügen einer ganzen neuen Variable inkl. vorgeschriebener Metadatenfelder
* `delete_meta()`: Zum Löschen von Metadateneinträgen. Geeignet bei fälschlich hinzugefügten Informationen oder fürs Testen. Achtung die Funktion sollte nicht verwendet werden, um nicht mehr aktuelle Umfrageitems zu entfernen. Dafür kann das Metadatenfeld `status` der jeweiligen Variable aktualisiert werden.
* `interactive_mode_on()`: Erlaubt es, Metadaten in einer interaktiven Umgebung mit Hilfestellungen in der Konsole zu bearbeiten.
* `interactive_mode_off()`: Deaktiviert den interaktiven Modus. Kann z.B. beim Skript-basierten hinzufügen von Metadaten genutzt werden. 

## Übersicht der Vignetten
Mit folgenden Befehlen können die Vignetten in R-Studio aufgerufen werden 

* `vignette("metadaten_abfragen", "metaSABSEB")`: Beschreibt, wie Metadaten abgefragt werden können. 
* `vignette("metadaten_bearbeiten", "metaSABSEB")`: Beschreibt, wie die Metadaten angepasst und ergänzt werden können

## Änderungen vorschlagen

Der `main`-Branch ist geschützt. Änderungen können deshalb nicht direkt auf diesem Branch vorgenommen werden, sondern müssen zunächst in einem separaten Branch erfolgen.

Anschliessend ist ein Pull Request zu erstellen. Für die Integration in den main-Branch muss dieser durch den Code-Owner genehmigt werden.

Bei Fragen steht Christina Arn <christina.arn@bi.zh.ch> zur Verfügung.
