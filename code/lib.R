theme_report <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
    ret <- ggplot2::theme_minimal(base_family = "Roboto-Regular",
                                  base_size = base_size, ...)
    ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                            margin=margin(b=strip_text_margin),
                                            family="Roboto-Bold")
    ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                               margin=margin(b=subtitle_margin),
                                               family="PT Sans")
    ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                            family="Oswald")
    ret
}

import_data <- function(){
    library(tidyverse)
    raw = read_csv2("~/Downloads/microdados_Enade_2017_portal_2018.10.09/3.DADOS/MICRODADOS_ENADE_2017.txt")
    
    nomes = readr::read_csv(here::here("data/nome-cursos-emec.csv"))
    ies = readr::read_csv2(here::here("data/ies_Brasil.csv")) %>% 
        select(-matches("CI"), -`Situação`)
    srt_respostas = readr::read_csv(here::here("data/valores_qe_tidy.csv"), 
                                    col_types = "ccccccc") %>% 
        select(pergunta, enunciado, chave, valor)
    
    raw %>% 
        filter(CO_UF_CURSO == 25) %>% 
        write_projectdata(nomes, ies, "data/enade_2017_pb.csv")
    
    raw %>% 
        filter(CO_IES == 2564) %>% 
        write_projectdata(nomes, ies, "data/enade_2017_ufcg.csv")
    
    dados_cg = raw %>% 
        filter(CO_MUNIC_CURSO == 2504009) %>% 
        write_projectdata(nomes, ies, "data/enade_2017_cg.csv")
    
    raw %>% 
        filter(CO_UF_CURSO == 25) %>% 
        append_str_respostas(srt_respostas) %>% 
        write_projectdata(nomes, ies, "data/enade_2017_pb-str.csv")
    
    raw %>% 
        filter(CO_IES == 2564) %>% 
        append_str_respostas(srt_respostas) %>% 
        write_projectdata(nomes, ies, "data/enade_2017_ufcg-str.csv")
    
    dados_cg = raw %>% 
        filter(CO_MUNIC_CURSO == 2504009) %>% 
        append_str_respostas(srt_respostas) %>% 
        write_projectdata(nomes, ies, "data/enade_2017_cg-str.csv")
}

append_str_respostas <- function(df, srt_respostas){
    qes = df %>% 
        select(starts_with("QE_")) %>% 
        mutate(id = 1:n()) 
    
    demais = df %>% 
        select(-starts_with("QE_")) %>% 
        mutate(id = 1:n()) 
    
    qes_l = qes %>% 
        gather(key = "questão", value = "resposta", -id) %>% 
        left_join(srt_respostas, by = c("questão" = "pergunta", "resposta" = "chave")) %>% 
        group_by(`questão`) %>% 
        mutate(enunciado = first(na.omit(enunciado))) %>% 
        ungroup()
    
    qes_l %>% 
        select(-`questão`, -resposta) %>% 
        spread(enunciado, valor) %>% 
        left_join(demais, by = "id")
}

import_codigo_valores <- function(){
    library(tidyverse)
    
    valores = read_csv2(
        here::here("data/valores_qe.csv"),
        col_types = cols(
            i = col_integer(),
            pergunta = col_character(),
            tipo = col_character(),
            tamanho = col_integer(),
            enunciado = col_character(),
            valores = col_character()
        )
    )
    
    valores_limpo = valores %>% 
        separate_rows(valores, sep = "\n") %>% 
        separate(valores, c("chave", "valor"), sep = "=") %>% 
        mutate(valor = str_replace(valor, "\\.", "")) %>% 
        mutate(valor = if_else(is.na(valor), "", valor)) %>% 
        mutate(valor = case_when(
            # as.numeric(chave) %in% c(1, 6) ~ paste(chave, "-", valor), 
            as.numeric(chave) %in% 1:6 ~ chave,
            as.numeric(chave) %in% c(7, 8) ~ NA_character_,
            TRUE ~ valor
        )) 
    
    valores_limpo %>% 
        write_csv(here::here("data/valores_qe_tidy.csv"))
}


write_projectdata <- function(enade, nomes, ies, saida){
    enade %>% 
        left_join(nomes, by = "CO_CURSO") %>% 
        filter(TP_PRES == 555, TP_PR_GER == 555) %>%
        select(-150:-138) %>% 
        select(-starts_with("NU_ITEM_"),
               -starts_with("DS_VT_"),
               -starts_with("TP_PR_"),
               -starts_with("TP_SFG")) %>% 
        left_join(ies, by = c("CO_IES" = "Código IES")) %>% 
        write_csv(here::here(saida))
}


read_projectdata <- function(){
    # readr::read_csv(here::here("data/enade_2017_cg.csv")) 
    readr::read_csv(here::here("data/enade_2017_ufcg.csv")) 
}
