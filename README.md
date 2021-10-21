# Pelatihan Penyiapan Data - Universitas Bengkulu

Materi ini akan membahas tahapan dan cara melakukan penyiapan data menggunakan package {`tidyverse`}. Untuk itu, Anda perlu install terlebih dahulu package {`tidyverse`} dan {`writexl`}.

```
install.packages(c("tidyverse", "writexl"))
```
Data yang sudah disiapkan berupa data transaksi retail dan _membership_-nya. Anda dapat unduh data melalui link yang ada pada file `data.md` dalam folder `data`.

Contoh kasus penyiapan data pada pelatihan ini adalah menyiapkan data berupa _Recency_, _Frequency_, dan _Monetary_ serta beberapa variable tambahan lain yang nantinya dapat digunakan untuk analisis lebih lanjut, seperti analisis RFM atau analisis gerombol (_Cluster Analysis_).

_Software_ yang digunakan pada pelatihan ini adalah program R versi 4.1.1 dan RStudio versi 2021.09 (terbaru). 

Package yang digunakan adalah {`tidyverse`} ({`dplyr`}, {`tidyr}`, {`stringr`}),{`readxl`}, {`lubridate`} dan {`writexl`}.

