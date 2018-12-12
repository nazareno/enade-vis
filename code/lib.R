theme_report <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 36,
                         plot_title_margin = 10,
                         ...) {
    ret <- ggplot2::theme_minimal(base_family = "Roboto-Regular",
                                  base_size = base_size, ...)
    ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                            margin=margin(b=strip_text_margin),
                                            family="Roboto-Regular")
    ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                               margin=margin(b=subtitle_margin),
                                               family="PT Sans")
    ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                            margin=margin(b=plot_title_margin),
                                            family="Oswald")
    # ret$panel.margin <- ggplot2::unit(4, "in")
    ret
}

import_data <- function(){
    library(tidyverse)
    raw = read_csv2("~/Downloads/microdados_Enade_2017_portal_2018.10.09/3.DADOS/MICRODADOS_ENADE_2017.txt")
    # raw = read_csv2("raw.tgz")
    
    nomes = readr::read_csv(here::here("data/nome-cursos-emec.csv"))
    ies = readr::read_csv2(here::here("data/ies_Brasil.csv")) %>% 
        select(-matches("CI"), -`Situação`)
    str_respostas = readr::read_csv(here::here("data/valores_qe_tidy.csv"), 
                                    col_types = "ccccccc") %>% 
        select(pergunta, enunciado, chave, valor)
    
    #
    # dados com os códigos
    #
    write_coded_datafiles(raw, nomes, ies)
    
    #
    # dados com strings das perguntas e respostas
    #
    write_str_datafiles(raw, nomes, ies, str_respostas)
    
    read_csv(here::here("data/enade_2017_ufcg-str.csv")) %>% 
        augment_str_datafiles() %>%
        write_csv(here::here("data/enade_2017_ufcg_medias.csv"))
    
    read_csv(here::here("data/enade_2017_ccc_br-str.csv")) %>% 
        augment_str_datafiles(tamanho_minimo = 50) %>% 
        write_csv(here::here("data/enade_2017_cccs_medias.csv"))
    
    read_csv(here::here("data/enade_2017_tudo_br-str.csv")) %>% 
        augment_str_datafiles(tamanho_minimo = 60) %>% 
        write_csv(here::here("data/enade_2017_tudo_medias.csv"))
}

augment_str_datafiles <- function(dados, tamanho_minimo = 20){
    codigos = readr::read_csv(here::here("data/valores_qe_tidy.csv")) %>% 
        select(pergunta, enunciado) %>% 
        unique()

    aumentado = dados %>% 
        mutate(NOME_CURSO = paste0(NOME_CURSO, " (", CO_CURSO, ")"), IES = Sigla) %>% 
        select(2:82, NOME_CURSO, IES, UF) %>% 
        gather(key = "enunciado", value = "resposta", -NOME_CURSO, -IES, -UF) %>% 
        left_join(codigos, by = "enunciado") %>% 
        mutate(pergunta = as.numeric(str_sub(pergunta, start = 5)))
    
    perguntas = tibble(pergunta = pull(aumentado, pergunta) %>% unique()) %>% 
        mutate(
            categoria = case_when(
                pergunta %in% c(1:3, 6, 9, 10, 22) ~ "Sobre o concluinte",
                pergunta %in% c(4, 5, 7, 8, 21) ~ "Sobre a família",
                pergunta %in% c(12:15, 24) ~ "Oportunidades e auxílios",
                pergunta %in% c(17:19, 20, 25, 26) ~ "Ensino médio, escolha e incentivo",
                pergunta %in% c(27:29, 31:36, 66) ~ "Aprendizagem além do técnico",
                pergunta %in% c(30, 37:39, 40, 23, 55:58) ~ "Ensino, apoio e avaliacão",
                pergunta %in% c(41:49, 52:54, 60) ~ "Curso em geral",
                pergunta %in% c(59, 61:65, 67:68) ~ "Infraestrutura",
                TRUE ~ "Outros"
            ), 
            tipo = case_when(
                pergunta %in% c(27:68) ~ "nota",
                TRUE ~ "categorias"
            )
        ) %>% 
        filter(
            pergunta <= 68
        )
    
    aumentado = aumentado %>% 
        filter(!is.na(resposta)) %>% 
        count(enunciado, pergunta, NOME_CURSO, IES, UF, resposta) %>% 
        group_by(enunciado, pergunta, NOME_CURSO, IES, UF) %>% 
        mutate(perc = n / sum(n)) %>% 
        ungroup() %>% 
        left_join(perguntas, by = "pergunta")

    # aumentado %>% 
    #     write_csv(here::here("data/enade_2017_ufcg_str_aug.csv"))
    
    aumentado %>% 
        filter(tipo == "nota") %>% 
        mutate(resposta = as.numeric(resposta)) %>% 
        group_by(NOME_CURSO, IES, UF, pergunta, categoria, enunciado) %>% 
        summarise(media = sum(as.numeric(resposta) * perc), n = sum(n)) %>% 
        filter(n >= tamanho_minimo) %>% 
        arrange(-media) %>%
        group_by(enunciado) %>% 
        mutate(rank = 1:n())
}

write_coded_datafiles <- function(raw, nomes, ies){
    raw %>% 
        filter(CO_UF_CURSO == 25) %>% 
        select_project_variables(nomes, ies) %>% 
        write_csv(here::here("data/enade_2017_pb.csv"))
    
    raw %>% 
        filter(CO_IES == 2564) %>% 
        select_project_variables(nomes, ies) %>% 
        write_csv(here::here("data/enade_2017_ufcg.csv"))
}

write_str_datafiles <- function(raw, nomes, ies, str_respostas){
    raw %>% 
        filter(CO_UF_CURSO == 25) %>% 
        append_str_respostas(str_respostas) %>% 
        select_project_variables(nomes, ies) %>% 
        write_csv(here::here("data/enade_2017_pb-str.csv"))
    
    raw %>% 
        filter(CO_IES == 2564) %>% 
        append_str_respostas(str_respostas) %>% 
        select_project_variables(nomes, ies) %>% 
        write_csv(here::here("data/enade_2017_ufcg-str.csv"))
    
    raw %>% 
        filter(CO_GRUPO == 4004) %>% 
        append_str_respostas(str_respostas) %>% 
        select_project_variables(nomes, ies) %>%  
        mutate(NOME_CURSO = 'Ciência Da Computação (Bacharelado)') %>%  ## TODO GAMBIARRA!
        write_csv(here::here("data/enade_2017_ccc_br-str.csv"))
    
    raw %>% 
        append_str_respostas(str_respostas) %>% 
        select_project_variables(nomes, ies) %>%  
        write_csv(here::here("data/enade_2017_tudo_br-str.csv"))
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
        filter(!(`questão` %in% paste0("QE_I", 69:81))) %>%
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


select_project_variables <- function(enade, nomes, ies){
    enade %>% 
        left_join(nomes, by = "CO_CURSO") %>% 
        filter(TP_PRES == 555, TP_PR_GER == 555) %>% 
        select(-starts_with("NU_ITEM_"),
               -starts_with("DS_VT_"),
               -starts_with("CO_RS"),
               -starts_with("NT_FG_"),
               -starts_with("NT_DIS_"),
               -starts_with("NT_OBJ_"),
               -starts_with("NT_CE_"),
               -starts_with("TP_")) %>% 
        left_join(ies, by = c("CO_IES" = "Código IES")) 
}


read_projectdata <- function(){
    # readr::read_csv(here::here("data/enade_2017_cg.csv")) 
    readr::read_csv(here::here("data/enade_2017_ufcg-str.csv")) 
}
