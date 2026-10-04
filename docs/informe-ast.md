# Informe de AST — Taller 1: polinomios dispersos


**Curso:** Fundamentos de Interpretación y Compilación de Lenguajes
de Programación — Universidad del Valle, Sede Tuluá.

**Integrantes del grupo:**

| Nombre | Código | Correo institucional |
|--------|--------|----------------------|
|Adriana Milena Noscue Dagua| 2477336|adriana.noscue@correounivalle.edu.co |
|Sebastian Cucalon Astorquiza| 2477344|sebastian.cucalon@correounivalle.edu.co |
|Santiago Torres Rojas|2380301 |santiago.torres.rojas@correounivalle.edu.co |
|Nicolle Camila Hoyos Puin|2380608 |nicolle.hoyos@correounivalle.edu.co |

---

## 1. Gramática considerada

Esta es la gramática del TAD polinomio; sus constructores son las etiquetas de los nodos de los árboles de la sección 2

```bnf
<polinomio>   ::= <variable> <terminos>
                   poli(var, terms)

<variable>    ::= <symbol>
                   nombre-var(s)

<terminos>    ::= '()
                   sin-terminos()
              ::= <termino> <terminos>
                   mas-terminos(term, resto)

<termino>     ::= <coeficiente> <exponente>
                   termino(coef, expo)

<coeficiente> ::= <int>
                   coef-ent(n)
              ::= <int> "/" <int>
                   coef-rac(num, den)

<exponente>   ::= <int>
                   expo-nat(k)
```

Indique cómo se realiza cada no terminal en su implementación con
`define-datatype`:

| No terminal | Variantes del datatype | Campos |
|---|---|---|
| `<polinomio>` | `poli` | `var` (variable), `terms` (términos) |
| `<terminos>` | `sin-terminos`, `mas-terminos` | `(ninguno)`<br>`term` (término), `resto` (términos) |
| `<termino>` | `termino` | `coef` (coeficiente), `expo` (exponente) |
| `<coeficiente>` | `coef-ent`, `coef-rac` | `n` (entero)<br>`num` (numerador), `den` (denominador) |
| `<exponente>` | `expo-nat` | `k` (entero no negativo) |

---

## 2. Ejemplos de AST

> Los cuatro ejemplos que siguen son los que pide el enunciado. Cada
> uno lleva el polinomio escrito en notación matemática, el AST como
> diagrama Mermaid con los nombres de los constructores en los nodos,
> y una explicación breve.
>
> El nodo del final de la lista de términos, `sin-terminos`, se dibuja
> siempre: es el caso base de la recursión y sin él el árbol queda
> incompleto.

### Ejemplo 1 — un solo término con coeficiente entero

**Polinomio:** $p_1 = {{7x^{3}}}$

**Construcción:**

```scheme
{{(poli (nombre-var 'x)
        (mas-terminos (termino (coef-ent 7) (expo-nat 3))
                      (sin-terminos)))}}
```

**AST:**

```mermaid
graph TD
  A[poli]
  A --> B[nombre-var: x]
  A --> C[mas-terminos]
  C --> D[termino]
  D --> E[coef-ent: 7]
  D --> F[expo-nat: 3]
  C --> G[sin-terminos]
```

**Explicación:** El nodo `nombre-var: x` indica la variable del polinomio. La lista de términos se compone de un único nodo `mas-terminos` que contiene la estructura `termino` y se cierra explícitamente con `sin-terminos` como caso base. El coeficiente (`coef-ent: 7`) y el exponente (`expo-nat: 3`) se separan en nodos individuales para mantener la abstracción y el desacoplamiento de la representación de datos.

---

### Ejemplo 2 — dos términos, uno con coeficiente racional

**Polinomio:** $p_2 = {{\frac{3}{4}x^{5} - 2x}}$

**Construcción:**

```scheme
(poli (nombre-var 'x)
      (mas-terminos (termino (coef-rac 3 4) (expo-nat 5))
                    (mas-terminos (termino (coef-ent -2) (expo-nat 1))
                                  (sin-terminos))))
```

**AST:**

```mermaid
graph TD
  A[poli]
  A --> B[nombre-var: x]
  A --> C[mas-terminos]
  C --> D[termino]
  D --> E[coef-rac: 3 / 4]
  D --> F[expo-nat: 5]
  C --> G[mas-terminos]
  G --> H[termino]
  H --> I[coef-ent: -2]
  H --> J[expo-nat: 1]
  G --> K[sin-terminos]
```

**Explicación:** El subárbol del coeficiente racional usa el nodo `coef-rac: 3 / 4` especificando numerador y denominador de forma independiente. El invariante de orden decreciente se evidencia en que el primer hijo `mas-terminos` alberga al término de mayor exponente ($5$), el cual apunta en su rama derecha al término de exponente menor ($1$).

---

### Ejemplo 3 — tres o más términos, con término independiente

**Polinomio:** $p_3 = 4x^{4} - \frac{1}{2}x^{2} + 9$

**Construcción:**

```scheme
(poli (nombre-var 'x)
      (mas-terminos (termino (coef-ent 4) (expo-nat 4))
                    (mas-terminos (termino (coef-rac -1 2) (expo-nat 2))
                                  (mas-terminos (termino (coef-ent 9) (expo-nat 0))
                                                (sin-terminos)))))
```

**AST:**

```mermaid
graph TD
  A[poli]
  A --> B[nombre-var: x]
  A --> C[mas-terminos]
  C --> D[termino]
  D --> E[coef-ent: 4]
  D --> F[expo-nat: 4]
  C --> G[mas-terminos]
  G --> H[termino]
  H --> I[coef-rac: -1 / 2]
  H --> J[expo-nat: 2]
  G --> K[mas-terminos]
  K --> L[termino]
  L --> M[coef-ent: 9]
  L --> N[expo-nat: 0]
  K --> O[sin-terminos]
```

**Explicación:** El término independiente ($9$) no requiere un constructor especial; se representa de forma estándar asignándole un exponente $0$ mediante el nodo `expo-nat: 0`, manteniendo así una estructura sintáctica homogénea en toda la lista de términos.

---

### Ejemplo 4 — el resultado de `(sumar p q)`

**Operandos** (los del ejemplo de la Parte 3 del enunciado):

- $p = 4x^{5} - \frac{3}{2}x^{2} + 7$
- $q = -4x^{5} + \frac{1}{2}x^{2} + 2x$

**Resultado:** $p + q = -x^{2} + 2x + 7$

**AST del resultado:**

```mermaid
graph TD
  A[poli]
  A --> B[nombre-var: x]
  A --> C[mas-terminos]
  C --> D[termino]
  D --> E[coef-ent: -1]
  D --> F[expo-nat: 2]
  C --> G[mas-terminos]
  G --> H[termino]
  H --> I[coef-ent: 2]
  H --> J[expo-nat: 1]
  G --> K[mas-terminos]
  K --> L[termino]
  L --> M[coef-ent: 7]
  L --> N[expo-nat: 0]
  K --> O[sin-terminos]
```

**Origen de cada nodo.**

| Término del resultado | Viene de | Observación |
|---|---|---|
| $-x^{2}$ | $p$ y $q$ | Se combinan $-\frac{3}{2}x^{2}$ (de $p$) y $\frac{1}{2}x^{2}$ (de $q$): $-\frac{3}{2} + \frac{1}{2} = -1$. Como el resultado es entero, el coeficiente es `coef-ent: -1` y no un `coef-rac`. |
| $2x$ | $q$ | Solo $q$ tiene término de grado 1. |
| $7$ | $p$ | Solo $p$ tiene término independiente. |

**Términos cancelados:** $4x^{5}$ (de $p$) y $-4x^{5}$ (de $q$) tienen el mismo exponente y $4 + (-4) = 0$. Por el invariante del TAD (sin coeficientes cero), el término desaparece y no tiene nodo en el árbol del resultado.

---

## 3. Referencias

- Friedman, D. P., & Wand, M. *Essentials of Programming Languages*,
  3.ª ed., MIT Press, 2008. Sección 2.1 (especificación de datos),
  sección 2.2 (representaciones de un TAD), sección 2.4
  (`define-datatype` y `cases`).
- Documentación oficial del lenguaje Racket (`#lang eopl`): https://docs.racket-lang.org/eopl/
