# Statistické zpracování vztahů mezi kvalitou ovzduší a dopravní situací v Bílině  

Tento repozitář obsahuje veškerý kód související s mou bakalářskou prací na téma **Statistické zpracování vztahů mezi kvalitou ovzduší a dopravní situací v Bílině**.  

📄 **Text bakalářské práce je dostupný na Overleaf:**  
[Overleaf projekt](https://overleaf.prf.ujep.cz/read/trsykccwhdyb#75538b)  

## Popis projektu  
Práce se zaměřuje na analýzu časových řad environmentálních a dopravních dat z města Bílina s cílem identifikovat sezónní vzory a vztahy mezi dopravou a kvalitou ovzduší. K analýze jsou využity metody zpracování časových řad a vizualizace dat.  

## 📂 Struktura projektu  
- **`nacteni_dat.R`** – kompletní načtení potřebných data frames, které je nutné mít připravené pro práci s modely, zobrazení dat a další analýzy.  
- **`prehled_dat.R`** – vizualizace rozložení proměnných, analýzy dat a výpočet kroskorelační funkce jednotlivých časových řad.  
- **`/modely/`** – obsahuje modely pro časové řady:  
  - **Sezónní modely** pro analýzu prachových částic.  
  - **Regresní modely**, kde závislou proměnnou je časová řada s koncentrací prachových částic.  
- **`/bp_text/`** – obsahuje textový dokument s postupem práce, shrnutím konzultací, vizualizací výstupů a doprovodnými poznámkami.  

## 📊 Použitá data
Data pochází z Datového centra Ústeckého kraje (DCÚK) a obsahují informace o dopravě, kvalitě ovzduší a meteorologických podmínkách v Bílině.
