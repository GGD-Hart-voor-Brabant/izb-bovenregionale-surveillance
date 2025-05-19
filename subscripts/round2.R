

# Subfunctie voor correct afronden (round rond een half af naar rechtsbijzijnde even getal, dus ene keer omhoog en andere keer omlaag)
round2 = function(x, digits = 1) {
  # # Stukje code als je wilt dat de waarde onder een bepaald getal met een ander aantal decimaal wordt afgerond dan grotere getallen
  # if (x > 3) {n = 0}
  # else {n = 1}
  # {digits = 0}
  # # Afronden
  scale<-10 ^ digits; trunc(x * scale + sign(x) * 0.5) / scale}
