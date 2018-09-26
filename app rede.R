library(shiny)

# which fields get saved 
fieldsAll <- c("escola", "nome_completo", "estado","cidade","endereco","mail","percep",
               "sistema","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","outro1","outro2",
               "Q1", "Q2", "Q3", "Q4", "Q5", "Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15",
               "Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25","Q26","Q27","Q28","Q29","Q30","Q31",
               "Q32","Q33","Q34","Q35","Q36", "R1","R2","R3","R4","R5","R6","R7","R8","R9")

# which fields are mandatory
fieldsMandatory <- c("escola", "nome_completo", "cidade","endereco","mail", "percep",
                     "sistema", "demanda")


# add an asterisk to an input label
labelMandatory <- function(label) {
     tagList(
          label,
          span("*", class = "mandatory_star")
     )
}

# get current Epoch time
epochTime <- function() {
     return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
     format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
     fileName <- sprintf("%s_%s.csv",
                         humanTime(),
                         digest::digest(data))
     
     write.csv(x = data, file = file.path(responsesDir, fileName),
               row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
     files <- list.files(file.path(responsesDir), full.names = TRUE)
     data <- lapply(files, read.csv, stringsAsFactors = FALSE)
     #data <- dplyr::rbind_all(data)
     data <- do.call(rbind, data)
     data
}

# directory where responses get stored
responsesDir <- file.path("M:/Nucleos/Inteligência de Mercado/Tarefas Ale/Quest/responses")

# CSS to use in the app
appCSS <-
     ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 50px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

# usernames that are admins
adminUsers <- c("admin", "prof")

# info for sharing this app on facebook/twitter
share <- list(
     title = "Pesquisa Percepcao 2018",
     image = "M:/Nucleos/Inteligência de Mercado/Tarefas Ale/Quest/Positivo.png",
     description = "Form."
)

shinyApp(
     ui = fluidPage(
          shinyjs::useShinyjs(),
          shinyjs::inlineCSS(appCSS),
          title = "Pesquisa Percepcao 2018",
          tags$style(HTML("
                          @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                          
                          h1 {font-family: 'Calibri'; color: #0E4AA2; font-size: 50px; background-color:white;font-weight: bold}
                          h2 {font-family: 'Calibri'; color: #3863A4;}
                          h3 {font-family: 'Calibri'; color: #3863A4;}
                          h4 {font-family: 'Calibri'; color: #3863A4;}
                          h5 {font-family: 'Calibri'; color: #0E4AA2; font-size: 20px}
                          h6 {font-family: 'Calibri'; color: #FF6666;}
                          label {font-family: 'Calibri'; color: #0E4AA2;font-size: 20px}
                          td {font-family: 'Calibri'; color: #CA6500;font-size: 16px}
                          tr {font-family: 'Calibri'; color: #0E4AA2;font-size:20px}
                          p {font-family: 'Calibri'; color: #3863A4;}
                          body {background-color: #D5DCE8;}
                          
                          ")),
          tags$head(
               
               # Facebook OpenGraph tags
               tags$meta(property = "og:title", content = share$title),
               tags$meta(property = "og:type", content = "website"),
               tags$meta(property = "og:url", content = share$url),
               tags$meta(property = "og:image", content = share$image),
               tags$meta(property = "og:description", content = share$description),
               
               # Twitter summary cards
               tags$meta(name = "twitter:card", content = "summary"),
               tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
               tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
               tags$meta(name = "twitter:title", content = share$title),
               tags$meta(name = "twitter:description", content = share$description),
               tags$meta(name = "twitter:image", content = share$image)
          ),
          tags$a(
               href="",
               tags$img(style="position: absolute; top: 0; right: 0; border: 0;")
          ),
          div(id = "header",
              h1("Pesquisa 2018"),
              h4("")
          ),
          
          fluidRow(
               
               column(2, h6("* Obrigatórias")),
               column(8,
                      div(
                           id = "form",
                           
                           
                           
                           textInput("escola", labelMandatory("Escola"), "", width="860px"),
                           textInput("nome_completo", labelMandatory("Seu nome completo"), width="860px"),
                           selectInput("estado", labelMandatory("Estado"),
                                       c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia",
                                         "Ceará", "Distrito Federal", "Espírito Santo", 
                                         "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul",
                                         "Minas Gerais", "Pará", "Paraíba", "Pernambuco",
                                         "Piauí", "Rio de Janeiro","Rio Grande do Norte", "Rio Grande do Sul",
                                         "Rondônia", "Roraima", "Santa Catarina", "São Paulo",
                                         "Sergipe", "Tocantins"), width="500px"),
                           textInput("cidade", labelMandatory("Cidade"), width="500px"),
                           textInput("endereco", labelMandatory("Endereço"),  width="800px", placeholder="exemplo: Rua Brasil, 1000"),
                           textInput("mail", "E-mail", width="500px", placeholder = "exemplo: meuemail@gmail.com"),
                           textAreaInput("percep",labelMandatory("Qual é a sua percepção sobre a Positivo? Quais são seus pontos fortes e em quais ela deveria melhorar?"), width="860px"),
                           textAreaInput("sistema",labelMandatory("Qual sistema de ensino você utiliza atualmente? Você utiliza algum outro material além do fornecido por ela?"), width="860px"),
                           # sliderInput("r_num_years", "Preco", 0, 100, 2, ticks = FALSE),
                           ###################################################################################
                           h5(strong("Definimos 12 atributos para entender o que é mais importante para nossos clientes na hora de decidir sobre a escolha de um sistema de ensino. Por favor, distribua 100 pontos entre estes atributos. Quanto mais importante um atributo, mais pontos ele deve receber. (É possível dar 0 ponto a qualquer atributo)")),
                           tags$table(class = "table",
                                      tags$thead(tags$tr(
                                           tags$th("Atributos"),
                                           tags$th("Pontos")
                                      )),
                                      tags$tbody(
                                           tags$tr(
                                                tags$td("Preço", width="300px"),
                                                tags$td(sliderInput("H1", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Qualidade do material didático"),
                                                tags$td(sliderInput("H2", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Proposta pedagógica"),
                                                tags$td(sliderInput("H3", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Marca (reconhecimento, tradição e inovação)"),
                                                tags$td(sliderInput("H4", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Atualização do material (frequência, temas abordados)"),
                                                tags$td(sliderInput("H5", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Ferramentas tecnológicas para professor e aluno (Sistema acadêmico, video aula, monitoria online, agenda online, ferramentas de comunicação, etc)"),
                                                tags$td(sliderInput("H6", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Suporte pedagógico (qualidade, frequência)"),
                                                tags$td(sliderInput("H7", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Portfólio de produtos (abrangência e adequação)"),
                                                tags$td(sliderInput("H8", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Serviços extra educacional (sistemas administrativos: folha de pagamento, RH, jurídico, TI)"),
                                                tags$td(sliderInput("H9", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Prazo de pagamento e parcelamento"),
                                                tags$td(sliderInput("H10", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Marketing para a escola (captação de alunos, retenção)"),
                                                tags$td(sliderInput("H11", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td("Contrapartidas para fechamento de proposta (Desconto)"),
                                                tags$td(sliderInput("H12", "", 0, 100, 0, ticks = TRUE, width="600px"))
                                           ),
                                           tags$tr(
                                                tags$td(strong("Total")),
                                                tags$td(verbatimTextOutput("value"))
                                                
                                           )
                                      )
                                      
                           ),
                           
                           #################################################################################################
                           h5(strong("Como você classificaria os players de mercado com relação a cada atributo, dando notas de 0 (pior) a 10 (melhor)? Por favor, considere que 10 é a qualificação de seu provedor ideal para determinada dimensão. Caso não tenha trabalhado com outro provedor, responda somente sobre o atual")),
                           tags$table(class = "table",
                                      tags$thead(tags$tr(
                                           tags$th("Atributos"),
                                           tags$th("Positivo"),
                                           tags$th(textInput("outro1","Outro 1, qual?", width="150px")),
                                           tags$th(textInput("outro2","Outro 2, qual?", width="150px"))
                                      )),
                                      tags$tbody(
                                           tags$tr(
                                                tags$td("Preço", width="350px"),
                                                tags$td(selectInput("Q1", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q2", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q3", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Qualidade do material didático"),
                                                tags$td(selectInput("Q4", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q5", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q6", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Proposta pedagógica"),
                                                tags$td(selectInput("Q7", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q8", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q9", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Marca (reconhecimento, tradição e inovação)"),
                                                tags$td(selectInput("Q10", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q11", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q12", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Atualização do material (frequência, temas abordados)"),
                                                tags$td(selectInput("Q13", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q14", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q15", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Ferramentas tecnológicas para professor e aluno (Sistema acadêmico, video aula, monitoria online, agenda online, ferramentas de comunicação, etc)"),
                                                tags$td(selectInput("Q16", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q17", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q18", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Suporte pedagógico (qualidade, frequência)"),
                                                tags$td(selectInput("Q19", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q20", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q21", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Portfólio de produtos (abrangência e adequação)"),
                                                tags$td(selectInput("Q22", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q23", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q24", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Serviços extra educacional (sistemas administrativos: folha de pagamento, RH, jurídico, TI)"),
                                                tags$td(selectInput("Q25", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q26", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q27", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Prazo de pagamento e parcelamento"),
                                                tags$td(selectInput("Q28", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q29", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q30", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Marketing para a escola (captação de alunos, retenção)"),
                                                tags$td(selectInput("Q31", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q32", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q33", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           ),
                                           tags$tr(
                                                tags$td("Contrapartidas para fechamento de proposta (Desconto)"),
                                                tags$td(selectInput("Q34", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q35", "", c("","1","2","3","4","5","6","7","8","9"), width="150px")),
                                                tags$td(selectInput("Q36", "", c("","1","2","3","4","5","6","7","8","9"), width="150px"))
                                           )
                                      )
                                      
                           ),
                           ###################################################################################
                           h5(strong("Queremos avaliar 8 produtos e serviços que são oferecidos por sistemas de ensino. Por favor, classifique do mais importante para o menos importante os produtos/serviços abaixo.")),
                           tags$table(class = "table", 
                                      tags$thead(tags$tr(
                                           tags$th("Atributos"),
                                           tags$th("Classificação")
                                      )),
                                      tags$tbody(
                                           tags$tr(
                                                tags$td("Agenda digital (canal online de comunicação com os pais e alunos)", width="800px"),
                                                tags$td(selectInput("R1", "", c("","1","2","3","4","5","6","7","8","9")))
                                           ),
                                           tags$tr(
                                                tags$td("Avaliação digital (provas padronizadas com correção automática)"),
                                                tags$td(selectInput("R2", "", c("","1","2","3","4","5","6","7","8","9")))
                                           ),
                                           tags$tr(
                                                tags$td("Correção de redação (envio de redação por foto para correção por monitores)"),
                                                tags$td(selectInput("R3", "", c("","1","2","3","4","5","6","7","8","9")))
                                           ),
                                           tags$tr(
                                                tags$td("Livro digital (livro com recursos interativos)"),
                                                tags$td(selectInput("R4", "", c("","1","2","3","4","5","6","7","8","9")))
                                           ),
                                           tags$tr(
                                                tags$td("Laboratório digital (simulação de experiências de física, química e biologia)"),
                                                tags$td(selectInput("R5", "", c("","1","2","3","4","5","6","7","8","9")))
                                           ),
                                           tags$tr(
                                                tags$td("Monitoria online (esclarecimento de dívidas de alunos por meio de plataforma digital)"),
                                                tags$td(selectInput("R6", "", c("","1","2","3","4","5","6","7","8","9")))
                                           ),
                                           tags$tr(
                                                tags$td("Trilha adaptativa (atividades com questões que mudam de acordo com o desempenho do aluno)"),
                                                tags$td(selectInput("R7", "", c("","1","2","3","4","5","6","7","8","9")))
                                           ),
                                           tags$tr(
                                                tags$td("Vídeo aulas (aulas/explicação de conteúdos por meio de vídeos)"),
                                                tags$td(selectInput("R8", "", c("","1","2","3","4","5","6","7","8","9")))
                                           ),
                                           tags$tr(
                                                tags$td("Plano de estudos (acompanhamento e planejamento de estudos para o aluno em uma plataforma online)"),
                                                tags$td(selectInput("R9", "", c("","1","2","3","4","5","6","7","8","9")))
                                           )
                                      )
                                      
                           ),
                           #################################################################################################
                           
                           textAreaInput("demanda",labelMandatory("Quais são as principais demandas dos pais para as escolas?"), width="860px"),
                           
                           actionButton("submit", strong("Concluir"), class = "btn-primary",width = "150px"),
                           shinyjs::hidden(
                                span(id = "submit_msg", "Submitting..."),
                                div(id = "error",
                                    div(br(), tags$b("Error: "), span(id = "error_msg"))
                                )
                           )
                      )
               ),
               
               shinyjs::hidden(
                    div(
                         id = "thankyou_msg",
                         h3("Sua resposta foi registrada com sucesso!"),
                         actionLink("submit_another", "Submeter outra resposta")
                    )
               )
          )
          ),
     server = function(input, output, session) {
          
          # Enable the Submit button when all mandatory fields are filled out
          observe({
               mandatoryFilled <-
                    vapply(fieldsMandatory,
                           function(x) {
                                !is.null(input[[x]]) && input[[x]] != ""
                           },
                           logical(1))
               mandatoryFilled <- all(mandatoryFilled)
               
               shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
          })
          
          # Gather all the form inputs (and add timestamp)
          formData <- reactive({
               data <- sapply(fieldsAll, function(x) input[[x]])
               data <- c(data, timestamp = epochTime())
               data <- t(data)
               data
          })    
          
          # When the Submit button is clicked, submit the response
          observeEvent(input$submit, {
               
               # User-experience stuff
               shinyjs::disable("submit")
               shinyjs::show("submit_msg")
               shinyjs::hide("error")
               
               # Save the data (show an error message in case of error)
               tryCatch({
                    saveData(formData())
                    shinyjs::reset("form")
                    shinyjs::hide("form")
                    shinyjs::show("thankyou_msg")
               },
               error = function(err) {
                    shinyjs::html("error_msg", err$message)
                    shinyjs::show(id = "error", anim = TRUE, animType = "fade")
               },
               finally = {
                    shinyjs::enable("submit")
                    shinyjs::hide("submit_msg")
               })
          })
          
          # submit another response
          observeEvent(input$submit_another, {
               shinyjs::show("form")
               shinyjs::hide("thankyou_msg")
          })
          
          # render the admin panel
          output$adminPanelContainer <- renderUI({
               if (!isAdmin()) return()
               
               div(
                    id = "adminPanel",
                    h2("Previous responses (only visible to admins)"),
                    downloadButton("downloadBtn", "Download responses"), br(), br(),
                    DT::dataTableOutput("responsesTable"), br(),
                    "* There were over 2000 responses by Dec 4 2017, so all data prior to that date was deleted as a cleanup"
               )
          })
          
          # determine if current user is admin
          isAdmin <- reactive({
               is.null(session$user) || session$user %in% adminUsers
          })    
          
          # Show the responses in the admin table
          output$responsesTable <- DT::renderDataTable({
               data <- loadData()
               data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
               DT::datatable(
                    data,
                    rownames = FALSE,
                    options = list(searching = FALSE, lengthChange = FALSE)
               )
          })
          
          output$value <- renderText({as.numeric(input$H1) + as.numeric(input$H2) + as.numeric(input$H3)+
                    as.numeric(input$H4) + as.numeric(input$H5) + as.numeric(input$H6)+
                    as.numeric(input$H7) + as.numeric(input$H8) + as.numeric(input$H9)+
                    as.numeric(input$H10) + as.numeric(input$H11) + as.numeric(input$H12)
          })
          
          # Allow user to download responses
          output$downloadBtn <- downloadHandler(
               filename = function() { 
                    sprintf("mimic-google-form_%s.csv", humanTime())
               },
               content = function(file) {
                    write.csv(loadData(), file, row.names = FALSE)
               }
          )    
     }
     )