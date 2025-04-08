# Statistické zpracování vztahů mezi kvalitou ovzduší a dopravní situací v Bílině  

Tento repozitář obsahuje veškerý kód související s mou bakalářskou prací na téma **Statistické zpracování vztahů mezi kvalitou ovzduší a dopravní situací v Bílině**.  

📄 **Text bakalářské práce je dostupný na Overleaf:**  
[Overleaf projekt](https://overleaf.prf.ujep.cz/read/trsykccwhdyb#75538b)  

## Popis projektu  
Práce se zaměřuje na analýzu časových řad environmentálních a dopravních dat z města Bílina s cílem identifikovat sezónní vzory a vztahy mezi dopravou a kvalitou ovzduší. K analýze jsou využity metody zpracování časových řad a vizualizace dat.  

## 📂 Struktura projektu  
- **`nacteni_dat.R`** – kompletní načtení potřebného data frame, který je nutný mít připravený pro práci s modely, zobrazení dat a další analýzy
- **`/data/`** – složka, která obsahuje uložená procesovaná data, která se načítají do běhu
- **`/analyza_dat/`** – obsahuje skripty zaměřující se na deskriptivní analýzu dat, agregační funkce  
- **`/obrazky/`** – obsahuje uložené grafové výstupy  
- **`/stare_skripty/`** – obsahuje prvotní zkoumání vztahů a časových řad
- **`/textove_soubory/`** – obsahuje textové dokumenty s postupem práce, shrnutím konzultací, vizualizací výstupů a doprovodnými poznámkami.  

## 📊 Použitá data
Data pochází z Datového centra Ústeckého kraje (DCÚK) a obsahují informace o dopravě, kvalitě ovzduší a meteorologických podmínkách v Bílině.
