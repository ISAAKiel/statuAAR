# Calculate stature estimation according to: Olivier et al 1978.

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex (Olivier et al 1978). Bone measures used:
Hum2, Hum1, Rad1b, Fem2, Tib1b

The bone measures used are clearly specified only for the side specific
formulas, e.g. "Femur (2)". In all other cases and in the text only the
bone, e.g. Femur, is used, without any specific number for the measure.
We consistently use only the clearly specified bone measures, e.g. Femur
2, in all cases.

The regression formulas are arranged hierarchically according to the
given correlation coefficient r, from combinations of different bone
measurements to individual bone measurements. The regression formulas
for men, separated by gender, are ignored: "In the multiple regression
equations we have used the average of the right and left size, judging
the asymmetry to be virtually negligible: it is only significant for the
right ulna" (Olivier et al 1978, 515). This results in 15 formula for
males and 14 formula for females, based on different combinations of
bone measures according to the respective hierarchy of the correlation
coefficient achieved. Consequently, stature of female and male
individuals is estimation on the basis of different bone measures.
Furthermore, individuals without sex determination cannot be calculated
by the mean of two values based of different parameters. We therefore
only provide the mean of female and male stature for comparison
purposes. Only the first applicable measure of the given hierarchy will
be used.

Returns a data.frame with:

- ind: individual identifyer (rownames),

- sex: as provided for calculation: m, f, indet.

- stature: estimated on the provided sex and bone measures,

- bone (measure(s)): bones used for calculation,

- if_female (stature): columns with alternative stature for three sex
  classes,

- if_male (stature),

- if_indet. (stature) and

- n_measures: number of bone measures included: e.g. 2 Fem2 (left,
  right) + 1 Tib1

## Usage

``` r
olivier_etal_1978(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Olivier G, Aaron C, Fully G, Tissier G (1978). “New estimations of
stature and cranial capacity in modern man.” *Journal of Human
Evolution*, **7**(6), 513–518. ISSN 0047-2484,
[doi:10.1016/S0047-2484(78)80020-7](https://doi.org/10.1016/S0047-2484%2878%2980020-7)
.

Olivier G, Tissier H (1975). “Estimation de la stature féminine d’après
les os longs des membres.” *Bulletins et Mémoires de la Société
d’Anthropologie de Paris*, **2**(4), 297–305.
[doi:10.3406/bmsap.1975.1821](https://doi.org/10.3406/bmsap.1975.1821) .

## Author

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Christoph Rinne <crinne@ufg.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame
x <- statuAAR::Rollet1888

# Create a unique identifier from Nr and Sex
x$id <- paste(x$Sex, x$Nr, sep = "_")

# Create & check the data frame of mesures concordance for Rollet 1888
measures.concordance <- statuAAR::measures.concordance.rollet1888
measures.concordance[measures.concordance$own != "",]
#>    short            long           own
#> 1   Fem1         Femur.1           Fem
#> 2  Fem1l    Femur.1.left    Femur.left
#> 3  Fem1r   Femur.1.right   Femur.right
#> 8  Fib1l   Fibula.1.left   Fibula.left
#> 9  Fib1r  Fibula.1.right  Fibula.right
#> 10  Hum1       Humerus.1           Hum
#> 14 Hum1l  Humerus.1.left  Humerus.left
#> 15 Hum1r Humerus.1.right Humerus.right
#> 19  Rad1        Radius.1           Rad
#> 23 Rad1l   Radius.1.left   Radius.left
#> 24 Rad1r  Radius.1.right  Radius.right
#> 28  Tib1         Tibia.1           Tib
#> 35 Tib1l    Tibia.1.left    Tibia.left
#> 36 Tib1r   Tibia.1.right   Tibia.right
#> 37  Uln1          Ulna.1          Ulna
#> 38 Uln1l     Ulna.1.left     Ulna.left
#> 39 Uln1r    Ulna.1.right    Ulna.right

# Prepare tabled data into a long list (statuaar_data_table)
dl.rollet <- statuAAR::prep.statuaar.data(x, d.form = "wide", ind = "id",
                measures.names = "own", sex = "Sex", stats = FALSE)

# Calculate stature estimation using this formula.
ol78.estimates <- statuAAR::getStature(c("ol78"), dl.rollet)

# Extract the corresponding data frame from the returned list object.
ol78.estimates[["ol78"]]
#> [1] "There is no usable bone measurement / indice available for the chosen formula."
```
