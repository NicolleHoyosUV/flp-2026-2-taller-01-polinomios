# Informe de corrección — Taller 1: polinomios dispersos

> **Plantilla de entrega.** Copie este archivo a
> `docs/informe-correccion.md` dentro del repositorio del grupo y
> reemplace los marcadores `{{...}}` con su contenido. **No elimine
> las secciones obligatorias.** No se aceptan PDF, DOCX ni imágenes
> insertadas: todo el documento debe ser Markdown, las fórmulas en
> LaTeX (`$...$` / `$$...$$`) y los diagramas, si los hay, en Mermaid.
>
> Las demostraciones se hacen una sola vez, sobre la estructura
> recursiva que define la gramática, porque la lógica de las funciones
> es la misma en las tres representaciones.

**Curso:** Fundamentos de Interpretación y Compilación de Lenguajes
de Programación — Universidad del Valle, Sede Tuluá.

**Integrantes del grupo:**

| Nombre | Código | Correo institucional |
|--------|--------|----------------------|
|Adriana Milena Noscue Dagua | 2477336 |adriana.noscue@correounivalle.edu.co |
|Sebastian Cucalon Astorquiza| 2477344|sebastian.cucalon@correounivalle.edu.co |
|Santiago Torres Rojas|2380301 |santiago.torres.rojas@correounivalle.edu.co |
|Nicolle Camila Hoyos Puin|2380608 |nicolle.hoyos@correounivalle.edu.co |

---

## 1. Marco formal

### 1.1 Corrección de programas recursivos

Sea $f : A \to B$ una función y $A$ un conjunto definido
recursivamente. Sea $P_f$ un programa recursivo en Racket que pretende
calcular $f$. Decimos que $P_f$ es correcto con respecto a su
especificación si se cumple:

$$
\forall a \in A \,:\, P_f(a) = f(a)
$$

La estrategia de demostración es **inducción estructural** sobre $A$.
Aquí $A$ es el conjunto de listas de términos que genera la gramática:

- **Caso base:** $a = \text{sin-terminos}()$, y se verifica
  $P_f(a) = f(a)$ directamente.
- **Caso inductivo:** $a = \text{mas-terminos}(t, r)$. Se asume la
  **hipótesis de inducción** $P_f(r) = f(r)$ sobre el resto de la
  lista y se demuestra $P_f(a) = f(a)$.

Si alguna de sus funciones quedó escrita con un acumulador en lugar de
recursión estructural, la corrección se argumenta con una invariante
del acumulador y no con la hipótesis de inducción: enuncie la
invariante, demuestre que vale al inicio, que cada paso la conserva y
que al terminar implica la post-condición.

### 1.2 El invariante de la representación

Las cuatro condiciones del enunciado se enuncian como una única
propiedad sobre polinomios. Sea $p$ un polinomio con términos
$t_1, t_2, \ldots, t_n$, donde $t_i = (c_i, e_i)$:

$$
\mathrm{Inv}(p) \equiv
\underbrace{\forall i < n : e_i > e_{i+1}}_{\text{orden estricto}}
\ \land\
\underbrace{\forall i : c_i \neq 0}_{\text{sin ceros}}
\ \land\
\underbrace{\forall i : e_i \in \mathbb{N}}_{\text{exponentes naturales}}
\ \land\
\underbrace{\forall i : \mathrm{red}(c_i)}_{\text{racionales reducidos}}
$$

donde $\mathrm{red}\left(\frac{a}{b}\right)$ abrevia
$b > 0 \,\land\, \mathrm{mcd}(|a|, b) = 1$, y un coeficiente entero se
toma como el racional de denominador $1$.

{{Si prefiere escribir el invariante con otra notación, hágalo, pero
las cuatro condiciones deben quedar todas y de forma que se puedan
verificar término por término.}}

---

## 2. Funciones analizadas

### 2.1 Corrección de `coeficiente-de`

**Especificación.**

- **Tipo:** `coeficiente-de : polinomio × exponente -> coeficiente`
- **Pre-condición:** $\mathrm{Inv}(p)$ y $e \in \mathbb{N}$ (el exponente es un entero no negativo).
- **Post-condición:** $\text{Post}(p, e, r) \equiv {{\ldots}}$ cuando
  el exponente $e$ aparece en $p$; y la función levanta
  `eopl:error` cuando no aparece.

**Código.**

```racket
; coeficiente-de : polinomio x exponente -> exact-number
; Propósito: Busca y retorna el coeficiente correspondiente al exponente dado en el polinomio.
(define coeficiente-de-aux
  (lambda (terms e)
    (if (sin-terminos? terms)
        (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente")
        (let ([actual-coef (concreto-coef (termino->coef (mas-terminos->term terms)))]
              [actual-expo (concreto-expo (termino->expo (mas-terminos->term terms)))])
          (cond
            [(= e actual-expo) actual-coef]
            [(< e actual-expo) (coeficiente-de-aux (mas-terminos->resto terms) e)]
            [else (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente")])))))

(define coeficiente-de
  (lambda (polinomio exponente)
    (cond
      [(or (not (integer? exponente)) (< exponente 0))
       (eopl:error 'coeficiente-de "El exponente debe ser un entero no negativo")]
      [else
       (coeficiente-de-aux (poli->terms polinomio) exponente)])))
```

**Demostración.**

- **Caso base** ($\text{sin-terminos}$): Cuando $\text{terms} = \text{sin-terminos}()$, no hay términos que evaluar. La condición `(sin-terminos? terms)` se evalúa como verdadera y la función ejecuta inmediatamente `(eopl:error ...)`. Esto cumple de manera exacta con la especificación de retornar error cuando el exponente no se encuentra presente.

 $$
 P_f(\text{sin-terminos}()) = \text{error} = f(\text{sin-terminos}())
 $$

- **Caso inductivo** ($\text{mas-terminos}(t, r)$): Sea $t = (c_{\text{act}}, e_{\text{act}})$. Se asume la hipótesis de inducción $P_f(r) = f(r)$ para la cola de términos $r$. Evaluamos tres casos:
  1. Si $e = e_{\text{act}}$, la función retorna $c_{\text{act}}$, cumpliendo la post-condición.
  2. Si $e < e_{\text{act}}$, por la H.I. la llamada recursiva `coeficiente-de-aux(r, e)` retorna $f(r)$, buscando correctamente el coeficiente en el resto de la lista.
  3. Si $e > e_{\text{act}}$, por el orden estrictamente decreciente garantizado por $\mathrm{Inv}(p)$, sabemos que $\forall t_j \in r, e_j < e_{\text{act}} < e$. Es imposible que $e$ se encuentre en $r$, por lo que se corta la búsqueda en $O(1)$ y se levanta `eopl:error` sin necesidad de recorrer el resto de la lista.

  $$
  P_f(\text{mas-terminos}(t, r)) = f(\text{mas-terminos}(t, r))
  $$

- **Levantamiento del error.** Se levanta el error si se alcanza $\text{sin-terminos}()$ o si $e > e_{\text{act}}$, casos donde es matemáticamente imposible que el término exista. Si el término existe, el orden estricto garantiza que la búsqueda se detendrá únicamente en el caso $e = e_{\text{act}}$, retornando el coeficiente sin arrojar error.

- **Terminación.** La medida es el número de términos en la lista, $\mu(\text{terms}) = \vert{}\text{terms}\vert{} \in \mathbb{N}$. En cada llamada recursiva la longitud disminuye estrictamente en $1$ ($\mu(r) = \mu(\text{terms}) - 1$), con cota inferior $0$.

**Conclusión:** La función `coeficiente-de` es totalmente correcta con respecto a su especificación.

---

### 2.2 Corrección de `eliminar-termino`

**Especificación.**

- **Tipo:** `eliminar-termino : polinomio × exponente -> polinomio`
- **Pre-condición:** $\mathrm{Inv}(p)$ y $e \in \mathbb{N}$ (entero no negativo).
- **Post-condición:** el resultado contiene **exactamente** los
  términos de $p$ menos el de exponente $e$. Formalmente:
  $$
  \text{terminos}(r) = \text{terminos}(p) \setminus \{(c, e)\}
  $$
  y la función levanta `eopl:error` si $e$ no aparece en $p$.

**Código.**

```racket
(define eliminar-termino-aux
  (lambda (terms e)
    (if (sin-terminos? terms)
        (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente")
        (let ([actual-expo (concreto-expo (termino->expo (mas-terminos->term terms)))])
          (cond
            [(= e actual-expo) (mas-terminos->resto terms)]
            [(< e actual-expo) (mas-terminos (mas-terminos->term terms)
                                             (eliminar-termino-aux (mas-terminos->resto terms) e))]
            [else (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente")])))))

(define eliminar-termino
  (lambda (polinomio exponente)
    (cond
      [(or (not (integer? exponente)) (< exponente 0))
       (eopl:error 'eliminar-termino "El exponente debe ser un entero no negativo")]
      [else
       (poli (poli->var polinomio)
             (eliminar-termino-aux (poli->terms polinomio) exponente))])))
```

**Demostración.** 
- **Caso base** ($\text{sin-terminos}$): Si $terms = \text{sin-terminos}()$, no hay términos que eliminar. Se ejecuta directamente `eopl:error`, lo cual satisface la post-condición de fallar cuando $e$ no está en el polinomio.

- **Caso inductivo** ($\text{mas-terminos}(t, r)$): Sea $t = (c_{\text{act}}, e_{\text{act}})$. Asumimos la H.I. de que `eliminar-termino-aux(r, e)` elimina correctamente el término de exponente $e$ de $r$.
  1. Si $e = e_{\text{act}}$, se retorna $r$, eliminando el primer término. Los términos resultantes corresponden exactamente a $\text{terminos}(p) \setminus \{(c, e)\}$.
  2. Si $e < e_{\text{act}}$, por H.I. la llamada recursiva elimina el término de $r$ devolviendo $r'$. Se reconstruye la lista como $\text{mas-terminos}(t, r')$.
  3. Si $e > e_{\text{act}}$, dado $\mathrm{Inv}(p)$, $e$ no está en $r$ y se levanta `eopl:error`.

- **Preservación del invariante:** Quitar un elemento de una secuencia decreciente produce una subsecuencia que conserva el orden estrictamente decreciente ($e_i > e_{i+1}$). Tampoco introduce coeficientes en cero ni altera los coeficientes racionales reducidos. Por ende, el resultado preserva $\mathrm{Inv}(p')$.

- **Terminación:** La medida $\mu(\text{terms}) = \vert{}\text{terms}\vert{}$ se reduce en $1$ en cada llamada sobre el resto de la lista, acotada inferiormente por $0$.

**Conclusión:** La función `eliminar-termino` es correcta y preserva el invariante.

---

### 2.3 `insertar-termino` preserva el invariante

**Enunciado.** Si $\mathrm{Inv}(p)$ vale antes de la llamada, entonces
$\mathrm{Inv}(\texttt{insertar-termino}(p, c, e))$ vale sobre el
resultado.

**Código.**

```racket
(define (insertar-termino p c e)
  ...)
```

**Demostración por casos.** Cubra los tres casos del enunciado y
verifique en cada uno las cuatro condiciones del invariante:

- **Caso A — el exponente es nuevo.** {{Dónde queda el término
  insertado y por qué el orden estricto se conserva. Qué pasa si el
  coeficiente que llega es cero.}}

- **Caso B — el exponente ya existía y la suma no es cero.** {{El
  término se reemplaza por uno con el coeficiente sumado; el orden no
  cambia porque el exponente es el mismo. Argumente que el coeficiente
  resultante queda reducido y con denominador positivo.}}

- **Caso C — el exponente ya existía y la suma es cero.** {{El término
  desaparece. Argumente que quitarlo conserva el orden estricto y que
  el resultado no queda con un cero, que es justo lo que exige la
  segunda condición.}}

**Terminación.** {{...}}

**Conclusión:** {{...}}

---

## 3. Equivalencia de las dos representaciones

Argumente por qué las funciones de la interfaz son las mismas para la
representación basada en listas y la basada en procedimientos, y qué
propiedad de la interfaz impide que el cliente las distinga. Basta una
explicación conceptual apoyada en la sección 2.2 de EOPL, sin
demostración formal.

Conviene que la explicación responda a esto:

- {{Qué ve el cliente de un polinomio: qué operaciones tiene
  disponibles y qué no puede hacer.}}
- {{Qué cambia entre las dos representaciones y por qué ese cambio
  queda del lado de adentro de la interfaz.}}
- {{Qué habría que hacer para que el cliente sí notara la diferencia,
  y por qué eso significaría que la abstracción se rompió.}}

---

## 4. Referencias

- Friedman, D. P., & Wand, M. *Essentials of Programming Languages*,
  3.ª ed., MIT Press, 2008. Sección 2.1 (especificación de datos),
  sección 2.2 (representación basada en listas y basada en
  procedimientos), sección 2.4 (`define-datatype` y `cases`).
- {{Otras referencias que hayan consultado.}}
