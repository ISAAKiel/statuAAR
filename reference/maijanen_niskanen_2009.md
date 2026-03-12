# Calculate stature estimation according to: Maijanen, Niskanen 2009

Stature estimation (mm) based on the hierarchy of different regression
calculations, separated by sex (Maijanen, Niskanen 2009). Bone measures
used: Fem2+Tib1, Fem1+Tib1, Fem2, Fem1, Tib1, Hum1, Rad1, Fib1, Uln1 The
addition of individual bone measurements is performed during the
calculation.

If bone measures for left and right are provided the mean value will be
used, but for statistic information 2 bones will be counted
(n_measures). Maijanen & Niskanen propose to use RMA based formula,
primarily the combination of femur and tibia, followed by calculations
on the hierarchie of single measures. In addition, they provide formula
for the combination of male and female individuals recomended in case of
undetermined or insecure sex determination (indet.). (2010, p. 479)
Unfortunately, tables 2 - 4 only provide the S.E. of the rejected least
square correlation (LSQ) to order the RMA parameter (slope, intercept).
In addition, the order derived from these S.E. differs for the first
foure bone measures:

- combined: Fem2+Tib1, Fem2, Fem1+Tib1, Fem1

- males: Fem2, Fem2+Tib1, Fem1, Fem1+Tib1

- females: Fem1+Tib1, Fem2+Tib1, Fem1, Fem2

- subsequent identical order: Fib1, Tib1, Hum1, Rad1, Uln1

In order to have a consistent calculation based on the same bone
measurements in cases of certain, uncertain, or even impossible sex
determination, the following order is applied to all individuals:
Fem2Tib1, Fem1Tib1, Fem2, Fem1, Fib1, Tib1, Hum1, Rad1, Uln1.

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
maijanen_niskanen_2009(df)
```

## Arguments

- df:

  data.frame of type statuaar_data_table, containing informations on
  individual, bone and measurement.

## Value

data.frame with calculated stature and related information per
individual.

## References

Maijanen H, Niskanen M (2010). “New regression equations for stature
estimation for medieval Scandinavians.” *International Journal of
Osteoarchaeology*, **20**(4), 472–480. ISSN 1099-1212,
[doi:10.1002/oa.1071](https://doi.org/10.1002/oa.1071) .

## Author

Christoph Rinne <crinne@ufg.uni-kiel.de>

Nils Müller-Scheeßel <nils.mueller-scheessel@ufg.uni-kiel.de>

Hendrik Raese <h.raese@ufg.uni-kiel.de>

Anna Loy <aloy@roots.uni-kiel.de>

## Examples

``` r
# Read example dataset into a data frame and check the columnames
x <- statuAAR::Rollet1888
colnames(x)
#>  [1] "Nr"              "Sex"             "Age"             "Stature"        
#>  [5] "Femur.right"     "Femur.left"      "Femur.right.dry" "Femur.left.dry" 
#>  [9] "Tibia.right"     "Tibia.left"      "Fibula.right"    "Fibula.left"    
#> [13] "Humerus.right"   "Humerus.left"    "Radius.right"    "Radius.left"    
#> [17] "Ulna.right"      "Ulna.left"      

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

# Calculate stature estimation using a given formula.
mn09.estimates <- statuAAR::getStature(c("mn09"), dl.rollet)

# Extract the corresponding data frame from the returned list object.
# Please note, not in all cases left and right measures are available.
mn09.estimates[["mn09"]]
#> [1] "There is no usable bone measurement / indice available for the chosen formula."
```
