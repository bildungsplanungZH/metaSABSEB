# Metadaten SAB/SEB

Repository erstellt am: 07.10.2025

**Kontakt**

- Christina Arn: <christina.arn@bi.zh.ch>
- Marco Schuppisser: <marco.schuppisser@zemces.ch>

<div style="background-color:#fff3cd; padding:10px; border-left:5px solid #ffc107;">
⚠️ Das Paket befindet sich noch in der Development Phase. ⚠️ <br><br>
Einige Features, Dokumentation und Tests sind immer noch in der Entwicklung.
</div>


### Kurzbeschreibung
Dieses R-Package [metaSABSEB](https://github.com/bildungsplanungZH/metaSABSEB) 
enthält **Metadaten** zum Qualitätsmonitoring Sek II. Es wurde durch die 
[Bildungsplanung des Kantons Zürich](https://www.zh.ch/de/bildungsdirektion/generalsekretariat-der-bildungsdirektion/bildungsplanung.html)
in Zusammenarbeit mit dem
[Schweizerischen Zentrum für die Mittelschule und für Schulevaluation auf Sekundarstufe ||](https://www.zemces.ch/de)
(ZEM CES) erarbeitet. 
Konkret geht es um Informationen zu folgenden Befragungen:

- Die standardisierte Abschlussklassenbefragung ([SAB](https://www.zemces.ch/de/evaluationen-und-befragungen/standardisierte-befragungen/abschlussklassenbefragung))
- Die standardisierte Ehemaligenbefragung ([SEB](https://www.zemces.ch/de/evaluationen-und-befragungen/standardisierte-befragungen/ehemaligenbefragung))

Die Metadaten beschreiben Spalten und Werteausprägungen und liefern wichtige 
Hintergrundinformationen zu den Daten.
Die gesammelten Metadaten beschränken sich auf die gesamtschweizerisch erhobenen 
Merkmale sowie auf Fokusmodule für den Kanton Zürich.
Metadaten zu den Fokusmodulen von anderen Kantonen wurden nicht berücksichtigt.


Darüber hinaus enthält das Package **Funktionen**, zum Abrufen, Anpassen und Ergänzen von Metadaten.

* `get_meta()`: Zum Abrufen von Metadaten; *erste Version verfügbar*
* `change_meta()`: Zum Ändern von einzelnen Metadatenfeldern; *erste Version verfügbar*
* `add_meta()`: Zum Hinzufügen einer ganzen neuen Variable inkl. vorgeschriebener Metadatenfelder; *work in progress* 



