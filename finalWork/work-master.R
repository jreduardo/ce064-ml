## =====================================================================
## Estratégias para classificação Binária: Um estudo de caso com
## classificação de e-mails
##                                                        Eduardo Junior
##                                                    edujrrib@gmail.com
##                                                            2016-07-04
## =====================================================================

##======================================================================
## Funções e pacotes utilizados

##-------------------------------------------
## Pacotes
pacotes <- list("caret", "MASS", "klaR", "mda", "mboost",
                "randomForest", "kernlab", "plyr", "pROC",
                "lattice", "latticeExtra")
sapply(pacotes, require, character.only = TRUE)

##-------------------------------------------
## Função para comparação dos resultados em tabela
tableCompare <- function(models) {
    ## models <- list(...)
    ##-------------------------------------------
    ## Para o AUC nos rept * k-fold
    cvs <- resamples(models)
    aucs <- apply(cvs$values[, grepl("ROC", names(cvs$values))],
                  MARGIN = 2, FUN = function(x) {
                      test <- t.test(x)
                      paste0(round(test$estimate, 3), " (",
                             paste(round(test$conf.int, 3),
                                   collapse = ", "),
                             ")")
                  })
    ##-------------------------------------------
    ## Para Acuracia, Especificidade, Sensitividade, PPV, NPV e AUC da
    ## classificação da base de teste
    res <- sapply(models, FUN = function(m) {
        res <- confusionMatrix(predict(m, spam.te), spam.te$type)
        accur <- paste0(round(res$overall["Accuracy"], 3), " (",
                        paste(round(res$overall["AccuracyLower"], 3),
                              round(res$overall["AccuracyUpper"], 3),
                              sep = ", "), ")")
        ##-------------------------------------------
        library(pROC)
        pred <- predict(m, spam.te, type = "prob")
        roc <- round(roc(spam.te$type, pred[, "spam"])$auc, 3)
        c("accur" = accur, round(res$byClass[1:4], 3), roc)
    })
    tab <- rbind(aucs, res)
    rownames(tab) <- c("AUC*", "Acurácia*", "Sensibilidade",
                       "Especificidade", "PPV", "NPV", "AUC")
    return(tab)
}

##-------------------------------------------
## Função para comparação dos resultados na curva ROC
curveCompare <- function(models, title.leg = NULL) {
    library(pROC)
    ## models <- list(...)
    rocs <- lapply(models, FUN = function(m) {
        pred <- predict(m, spam.te, type = "prob")
        roc(spam.te$type, pred[, "spam"])
    })
    if (is.null(names(rocs))) {
        names(rocs) <- paste("Model", 1:length(models))
    }
    aux <- lapply(rocs, FUN = function(roc)
        cbind(esp = roc$specificities, sens = roc$sensitivities)
        )
    da <- plyr::ldply(aux, .id = "model")
    ##-------------------------------------------
    ## Com a lattice
    xyplot(esp ~ 1-sens,
           groups = model,
           data = da,
           type = c("g", "l"),
           xlab = "1 - Especificidade",
           ylab = "Sensibilidade",
           auto.key = list(
               title = title.leg,
               cex.title = 1,
               lines = TRUE,
               points = FALSE,
               corner = c(0.9, 0.2))) +
        latticeExtra::layer(
            panel.abline(0, 1, col = "gray30")
        )
}


## ------------------------------------------------------------------------

##-------------------------------------------
## Carregando o dataset
data(spam, package = "kernlab")
levels(spam$type) <- c("não.spam", "spam")

##-------------------------------------------
## Dividindo a base
prop <- 0.70

set.seed(20124689)
spam <- spam[order(spam$type), ]
tab <- c(with(spam, table(type)))
index1 <- sample(1:tab[1], size = tab[1] * prop)
index2 <- sample((tab[1] + 1):sum(tab), size = tab[2] * prop)

spam.tr <- spam[c(index1, index2), ]
spam.te <- spam[-c(index1, index2), ]


## ----pie, fig.height = 6, out.width="0.4\\textwidth", fig.pos="H", fig.cap="Proporção de e-mails classificados como spams e não-spams"----

## Proporção de e-mails spam e não spam
cols <- trellis.par.get("superpose.line")$col[1:2]
props <- round(tab/sum(tab), 2)
labels <- paste0(c("não-spam", "spam"), " - ", props*100, "%")
pie(tab, labels = labels, col = c("#406DAC", "gray60"))

## ---- eval=FALSE, include=FALSE------------------------------------------

##======================================================================
## Obtendo os classificadores

library(caret)

## Define 5 repetições da validação cruzada 10-fold
cvCtrl <- trainControl(method = "repeatedcv",
                       repeats = 3,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)

## Essa etapa é bastante demorada! Para ajuste de todos os
## classificadores leva em torno de 1h

##----------------------------------------------------------------------
## DISCRIMINANT BASED

set.seed(20124689)
DB.linear <- train(
    type ~ .,
    data = spam.tr,
    method = "lda",
    metric = "ROC",
    trControl = cvCtrl
)

set.seed(20124689)
DB.quadratic <- train(
    type ~ .,
    data = spam.tr,
    method = "qda",
    metric = "ROC",
    trControl = cvCtrl
)

set.seed(20124689)
DB.regularized <- train(
    type ~ .,
    data = spam.tr,
    method = "rda",
    tuneLength = 3,
    metric = "ROC",
    trControl = cvCtrl
)

set.seed(20124689)
DB.penalized <- train(
    type ~ .,
    data = spam.tr,
    method = "pda",
    tuneLength = 10,
    metric = "ROC",
    trControl = cvCtrl
)

##----------------------------------------------------------------------
## GLM BASED

set.seed(20124689)
GB.glm <- train(
    type ~ .,
    data = spam.tr,
    method = "glm",
    metric = "ROC",
    trControl = cvCtrl
)

set.seed(20124689)
GB.boost <- train(
    type ~ .,
    data = spam.tr,
    method = "glmboost",
    tuneLength = 3,
    metric = "ROC",
    trControl = cvCtrl
)

##----------------------------------------------------------------------
## TREES BASES

set.seed(20124689)
TB.bagging <- train(
    type ~ .,
    data = spam.tr,
    method = "treebag",
    metric = "ROC",
    trControl = cvCtrl
)

set.seed(20124689)
TB.forest <- train(
    type ~ .,
    data = spam.tr,
    method = "rf",
    metric = "ROC",
    trControl = cvCtrl
)

##----------------------------------------------------------------------
## SVM BASED

set.seed(20124689)
SB.linear <- train(
    type ~ .,
    data = spam.tr,
    method = "svmLinear",
    tuneGrid = data.frame(
        C = seq(0.01, 2, length.out = 5)
    ),
    metric = "ROC",
    trControl = cvCtrl
)

set.seed(20124689)
SB.poly <- train(
    type ~ .,
    data = spam.tr,
    method = "svmPoly",
    metric = "ROC",
    trControl = cvCtrl
)

set.seed(20124689)
SB.radial <- train(
    type ~ .,
    data = spam.tr,
    method = "svmRadial",
    tuneLength = 5,
    metric = "ROC",
    trControl = cvCtrl
)



## ------------------------------------------------------------------------
## load("classifiers.rda")

models.final <- list()

## ----grafDB, fig.cap="(Esquerda) Intervalos de confiança para a área abaixo da curva ROC baseados nas 3 repetições das 10 amostras de validação cruzada. (Direita) Curva ROC dos classificados aplicados à base de teste."----

##----------------------------------------------------------------------
## Results of Discriminant Analysis Based

DBs <- list(DB.linear, DB.quadratic, DB.regularized, DB.penalized)
names(DBs) <- c("LDA", "QDA", "RDA", "PDA")

curDB <- curveCompare(DBs, title.leg = "Método")
rocDB <- dotplot(resamples(DBs), metric = "ROC")
gridExtra::grid.arrange(
    update(rocDB, sub = NULL, xlab = "AUC"),
    curDB, ncol = 2)
(tabDB <- tableCompare(DBs))

## Melhor desempenho
models.final$"LDA" <- DB.linear

## ----grafGB, fig.cap="(Esquerda) Intervalos de confiança para a área abaixo da curva ROC baseados nas 3 repetições das 10 amostras de validação cruzada. (Direita) Curva ROC dos classificados aplicados à base de teste."----

##----------------------------------------------------------------------
## Results of GLM Analysis Based

GBs <- list(GB.glm, GB.boost)
names(GBs) <- c("GLM-MLE", "GLM-Boost")

curGB <- curveCompare(GBs, title.leg = "Método")
rocGB <- dotplot(resamples(GBs), metric = "ROC")
gridExtra::grid.arrange(
    update(rocGB, sub = NULL, xlab = "AUC"),
    curGB, ncol = 2)
(tabGB <- tableCompare(GBs))

## Melhor desempenho
models.final$"GLM-MLE" <- GB.glm

## ----grafTB, fig.cap="(Esquerda) Intervalos de confiança para a área abaixo da curva ROC baseados nas 3 repetições das 10 amostras de validação cruzada. (Direita) Curva ROC dos classificados aplicados à base de teste."----

##----------------------------------------------------------------------
## Results of Decision Trees Analysis Based

TBs <- list(TB.bagging, TB.forest)
names(TBs) <- c("Tree-BAG", "Rand-Forest")

curTB <- curveCompare(TBs, title.leg = "Método")
rocTB <- dotplot(resamples(TBs), metric = "ROC")
gridExtra::grid.arrange(
    update(rocTB, sub = NULL, xlab = "AUC"),
    curTB, ncol = 2)
(tabTB <- tableCompare(TBs))

## Melhor desempenho
models.final$"Rand-Forest" <- TB.forest

## ----grafSB, fig.cap="(Esquerda) Intervalos de confiança para a área abaixo da curva ROC baseados nas 3 repetições das 10 amostras de validação cruzada. (Direita) Curva ROC dos classificados aplicados à base de teste."----

##----------------------------------------------------------------------
## Results of SVM Analysis Based

SBs <- list(SB.linear, SB.poly, SB.radial)
names(SBs) <- c("SVM-Linear", "SVM-Poly", "SVM-Gauss")

curSB <- curveCompare(SBs, title.leg = "Método")
rocSB <- dotplot(resamples(SBs), metric = "ROC")
gridExtra::grid.arrange(
    update(rocSB, sub = NULL, xlab = "AUC"),
    curSB, ncol = 2)
(tabSB <- tableCompare(SBs))

## Melhor desempenho
models.final$"SVM-Gauss" <- SB.radial

## ---- include=FALSE------------------------------------------------------

cvFINAL <- resamples(models.final)
curFINAL <- curveCompare(models.final)
xyFINAL <- update(
    splom(cvFINAL, metric = "ROC"),
    xlab = "Dispersão dos valores de AUC",
    main = "",
    ## varname.col = trellis.par.get("superpose.line")$col[2],
    varname.cex = 0.8,
    axis.text.cex = 0.5,
    axis.text.col = rgb(0.2, 0.2, 0.2, 0.6),

)
dotFINAL <- update(
    dotplot(cvFINAL),
    layout = c(1, NA),
    scales = "free",
    xlab = c("", "", ""),
    sub = NULL,
    strip = strip.custom(
        factor.levels = c(
            "AUC", "Sensibilidade", "Especificidade")
    )
)

(parFINAL <- update(
    parallelplot(cvFINAL, metric = "ROC"),
    xlab = "AUC",
    scales = list(y = list(alternating = 2))
))

(tabFINAL <- tableCompare(models.final))

## ----final1, fig.pos="ht", fig.cap="(Esquerda) Curva ROC dos classificadores aplicados à base de teste. (Direita) Gráficos de dispersão dos valores de AUC obtidos para cada uma das 30 amostras da validação cruzada."----

gridExtra::grid.arrange(curFINAL, xyFINAL, ncol = 2)


## ----final2, fig.pos="H", fig.height=6, fig.width=7.1, out.width="0.8\\linewidth", fig.cap="(Esquerda) Intervalos de confiança para especificidade, sensibilidade e área abaixo da curva ROC. (Direita) Valores de AUC. Ambos baseados nas 3 repetições das 10 amostras de validação cruzada"----

gridExtra::grid.arrange(dotFINAL, parFINAL, ncol = 2)
