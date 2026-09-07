# Metadaten SAB/SEB

Repository erstellt am: 07.10.2025
Release v0.1 am: 04.09.2026

**Kontakt**

- Christina Arn: <christina.arn@bi.zh.ch>
- Marco Schuppisser: <marco.schuppisser@zemces.ch>

## Kurzbeschreibung

Das R-Package [metaSABSEB](https://github.com/bildungsplanungZH/metaSABSEB) enthält **Metadaten** zum Qualitätsmonitoring Sek II. Es wurde durch die [Bildungsplanung des Kantons Zürich](https://www.zh.ch/de/bildungsdirektion/generalsekretariat-der-bildungsdirektion/bildungsplanung.html) in Zusammenarbeit mit dem
[Schweizerischen Zentrum für die Mittelschule und für Schulevaluation auf Sekundarstufe ||](https://www.zemces.ch/de) (ZEM CES) erarbeitet. 

Konkret geht es um Informationen zu folgenden Befragungen:

- Standardisierte Abschlussklassenbefragung ([SAB](https://www.zemces.ch/de/evaluationen-and-befragungen/befragungen/standardisierte-abschlussklassenbefragung-sab))
- Standardisierte Ehemaligenbefragung ([SEB](https://www.zemces.ch/de/evaluationen-and-befragungen/befragungen/standardisierte-ehemaligenbefragung-seb))

Die Metadaten beschreiben Spalten und Werteausprägungen und liefern wichtige Hintergrundinformationen zu den Daten.
Die gesammelten Metadaten beschränken sich auf die gesamtschweizerisch erhobenen Merkmale sowie auf einzelne Wahlmodule.

Darüber hinaus enthält das Package **Funktionen**, zum Abrufen, Anpassen und Ergänzen von Metadaten.

* `get_meta()`: Zum Abrufen von Metadaten
* `change_meta()`: Zum Ändern von einzelnen Metadatenfeldern von Variablen, die bereits erfasst wurden
* `add_meta()`: Zum Hinzufügen einer ganzen neuen Variable inkl. vorgeschriebener Metadatenfelder
* `delete_meta()`: Zum Löschen von Metadateneinträgen. Geeignet bei fälschlich hinzugefügten Informationen oder fürs Testen. Achtung die Funktion sollte nicht genuzt werden, um nicht mehr aktuelle Umfrageitems zu entfernen. Dafür kann das Metadatenfeld `status` der jeweiligen Variable aktualisiert werden.

Eine genaue Anleitung zur Nutzung der Funktionen findet sich in den Vignetten zum R-Package. 

## Installation

Das R-Package kann direkt in R mit folgendem Befehl installiert werden:
```
devtools::install_github("bildungsplanungZH/metaSABSEB", build_vignettes = T)
```

## Änderungen vorschlagen

Der main-Branch ist geschützt. Änderungen können nicht direkt darauf vorgenommen werden, sondern müssen zunächst in einem separaten Branch erfolgen. Anschliessend ist ein Pull-Request zu erstellen. Für eine Integration in den main-Branch muss dieser durch den Code-Owner approved werden. Bei Fragen steht Christina Arn (<christina.arn@bi.zh.ch>) zur Verfügung.
