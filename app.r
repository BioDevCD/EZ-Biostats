# =============================================================================
# MÓDULO 0: DEPENDENCIAS Y CONFIGURACIÓN GLOBAL
# =============================================================================
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggpubr)
library(ggprism)
library(rstatix)
library(viridis)
library(shinyjs)
library(DT)
library(bslib)
library(ggbeeswarm)
library(rio)
library(multcompView)
library(thematic)

# Configuración de opciones globales
# Aumentar límite de carga a 30MB para soportar datasets de laboratorio pesados
options(shiny.maxRequestSize = 30 * 1024^2)

# Configuración de codificación para prevenir errores con caracteres especiales
# (µ, α, β)
options(encoding = "UTF-8")

# =============================================================================
# MÓDULO 1: INTERFAZ DE USUARIO (UI) - REVISADO
# =============================================================================
ui <- fluidPage(
  # Inyección de CSS para evitar solapamientos y mejorar UX
  tags$head(
    tags$style(HTML("
      /* Errores de validación con estilo de alerta */
      .shiny-output-error-validation { 
        color: #E74C3C; 
        font-weight: bold; 
        padding: 10px;
        border: 1px solid #E74C3C;
        border-radius: 5px;
        background-color: #FDEDEC;
      }
      /* Posicionamiento del Dark Mode Switch */
      #dark_mode_container { 
        position: absolute; 
        top: 15px; 
        right: 20px;
        z-index: 1100; 
      }
      /* Scroll independiente para la barra lateral en pantallas pequeñas */
      .sidebar { 
        max-height: 95vh; 
        overflow-y: auto; 
      }
      /* Ajustes para dispositivos móviles */
      @media (max-width: 768px) {
        #dark_mode_container { 
          position: relative; 
          top: 0; 
          right: 0;
          float: right; 
          margin-bottom: 10px; 
        }
      }
      #plot_final {
        background-color: transparent !important;
      }
      .shiny-plot-output {
        background-color: transparent !important;
      }
      /* Color de texto que reacciona al modo oscuro */
      .custom-footer {
        color: var(--bs-secondary-color) !important;
        transition: color 0.3s ease;
      }
      /* Links y botones de acción en el footer con alto contraste */
      .custom-footer a, 
      .custom-footer .action-button, 
      .custom-footer #show_method { 
        color: var(--bs-link-color) !important;
        text-decoration-color: var(--bs-link-color) !important;
        transition: all 0.3s ease;
      }
      /* Cambio de color al pasar el mouse (hover) */
      .custom-footer a:hover, 
      .custom-footer #show_method:hover {
        color: var(--bs-link-hover-color) !important;
        filter: brightness(1.2);
      }
      /* Aviso de seguridad un poco más tenue (terciario) */
      .footer-warning {
        color: var(--bs-tertiary-color) !important;
      }
      /* Línea separadora (HR) sutil */
      .custom-footer hr {
        border-top: 1px solid var(--bs-border-color);
        opacity: 0.2;
      }
    "))
  ),

  # Definición del tema con bslib (Bootstrap 5)
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#2C3E50",
    base_font = font_google("Inter")
  ),

  # Contenedor del interruptor de modo oscuro
  tags$div(
    id = "dark_mode_container",
    uiOutput("darkmode_switch_ui")
  ),

  # Activación de dependencias de JavaScript
  useShinyjs(),

  # Salida dinámica para el título (permite cambio de idioma)
  uiOutput("dinamic_title"),

  sidebarLayout(
    sidebarPanel(
      class = "sidebar",

      # Selector de idioma oculto (controlado por el servidor)
      conditionalPanel(
        condition = "false",
        radioButtons("idioma", "", choices = c("EN", "ES"), selected = "EN")
      ),

      # Componentes dinámicos de entrada
      uiOutput("ui_archivo"),
      hr(),

      uiOutput("select_vars"),
      hr(),

      uiOutput("ui_custom_labels"),
      hr(),

      # Botonera de acción y descarga
      uiOutput("ui_boton_procesar"),
      uiOutput("ui_boton_reset"),
      uiOutput("download_ui"),

      hr(),
      tags$div(
        style = "margin-top: 15px; opacity: 0.8;",
        tags$small(
          style = "color: #7f8c8d; font-style: italic; line-height: 1.2;
          display: block;",
          uiOutput("ui_privacy_text")
        )
      )
    ),

    mainPanel(
      # Área principal condicionada a la carga de datos
      uiOutput("ui_tabs")
    )
  ),

  uiOutput("ui_footer")
)

# ===========================================================================
# MÓDULO 2: LÓGICA DEL SERVIDOR - IDIOMA Y DICCIONARIOS (AUDITADO)
# ===========================================================================

server <- function(input, output, session) {

  thematic::thematic_shiny(font = "auto")
  v_clic <- reactiveVal(0)
  idioma_rv <- reactiveVal("EN")
  current_year <- format(Sys.Date(), "%Y")

  # 2.1 Detección automática robusta (Prioridad Inglés)
  observe({
    browser_lang <- session$request$HTTP_ACCEPT_LANGUAGE
    if (!is.null(browser_lang) && grepl("^es", browser_lang,
                                        ignore.case = TRUE)) {
      idioma_rv("ES")
      updateRadioButtons(session, "idioma", selected = "ES")
    }
  }, priority = 10)

  # 2.2 Diccionario Maestro Reactivo
  lang <- reactive({
    req(idioma_rv())
    if (idioma_rv() == "ES") {
      list(
        app_title = "EZ Biostats - Software de Análisis Bioestadístico Inteligente", # nolint: line_length_linter.
        ver = "Versión Beta - Creado por BQ. C. Díaz",
        step1 = "Cargar Base de Datos (Excel, CSV, txt)",
        browse = "Examinar...",
        placeholder = "Ningún archivo seleccionado",
        reset = "Reiniciar",
        processing = "Procesando...",
        run = "Ejecutar Análisis",
        v_x = "Factor (Variable X):",
        v_y = "Variable Respuesta (Variable Y):",
        cust_title = "Personalizar Etiquetas (Gráfico):",
        lab_x = "Etiqueta Eje X:",
        lab_y = "Etiqueta Eje Y:",
        tab_data = "Vista de Datos",
        tab_plot = "Gráfico Final",
        tab_rep = "Reporte de Análisis",
        err_num = "Error: La Variable Respuesta seleccionada no es numérica.",
        err_factor = "Error: El Factor debe tener al menos 2 grupos.",
        err_file = "Por favor, carga un archivo válido.",
        err_n_small = "Error: Tamaño de muestra insuficiente (n < 3).",
        norm = "Normalidad (Shapiro-Wilk)",
        homo = "Homocedasticidad (Levene/Brown-Forsythe)",
        sup_ok = "Supuestos cumplidos. Se usó prueba paramétrica.",
        sup_fail = "Supuestos no cumplidos. Se usó prueba no paramétrica.", # nolint: line_length_linter.
        crit_dec = "Si p > 0.05, el supuesto se cumple.",
        tit_rep = "REPORTE DE ANÁLISIS BIOESTADÍSTICO",
        var_ana = "Variable Analizada",
        grupos  = "Grupos comparados",
        prueba  = "Prueba aplicada",
        pval    = "Valor p (exacto)",
        sig     = "Significancia",
        interp  = "Interpretación",
        pos     = "Existe evidencia suficiente para rechazar la hipótesis nula (p < 0.05). Se detectaron diferencias estadísticamente significativas.", # nolint: line_length_linter.
        neg     = "No existe evidencia suficiente para rechazar la hipótesis nula (p >= 0.05). No se detectaron diferencias significativas.", # nolint: line_length_linter.
        msd     = "(Media ± DE)",
        med_iqr = "(Mediana [IQR])",
        wide_toggle = "Mis datos están en Formato Ancho.", # nolint: line_length_linter.
        wide_help   = "Formato Ancho: Es cuando los títulos de las columnas son los nombres de los grupos. Ej: Columna A para 'Control', Columna B para 'Tratamiento', etc.", # nolint: line_length_linter.
        out_check = "Filtrar Outliers (Método IQR)",
        out_rep = "DETECCIÓN DE OUTLIERS (TUKEY)",
        out_method = "Criterio: Rango Intercuartílico (1.5 x IQR)",
        out_count = "Outliers detectados",
        out_rem = "Decisión: Filtrar. Se mantiene gráfico de Cajas para preservar la trazabilidad de la muestra y visualizar los valores excluidos.", # nolint: line_length_linter.
        out_keep = "Decisión: Mantener. Se usa gráfico de Cajas debido a la dispersión detectada.", # nolint: line_length_linter.
        out_clean = "Estado: Datos consistentes. Se justifica uso de gráfico de Barras por normalidad y ausencia de outliers.", # nolint: line_length_linter.
        out_mask_tit = "Enmascaramiento detectado:",
        out_mask_msg = "Tras el filtrado inicial, la reducción de varianza reveló %d outlier(s) adicional(es) en la muestra limpia. Estos valores se mantuvieron en el análisis estadístico.", # nolint: line_length_linter.
        pair_comp = "COMPARACIONES MÚLTIPLES (POST-HOC)",
        just = "Justificación",
        test_t = "Prueba t de Student",
        test_w = "Prueba t de Welch",
        test_wilcox = "Prueba de Wilcoxon (U de Mann-Whitney)",
        test_anova = "ANOVA de una vía",
        test_wanova = "ANOVA de Welch",
        test_kw = "Prueba de Kruskal-Wallis",
        ph_tukey = "Prueba de Tukey HSD",
        ph_gh = "Prueba de Games-Howell",
        ph_dunn = "Prueba de Dunn",
        reason_n = "Muestra pequeña (n < 5).",
        reason_norm = "Distribución no paramétrica.",
        reason_ok = "Supuestos cumplidos.",
        grupos_label = "Comparación:",
        prueba_label = "Método Estadístico:",
        just_label   = "Justificación:",
        priv_note = "Privacidad: Datos procesados solo en memoria (RAM). No se guardan datos ni archivos.",  # nolint: line_length_linter.
        tit_obs = "OBSERVACIONES METODOLÓGICAS",
        warn_n_crit = "CRÍTICO: n < 5. Los resultados son exploratorios; la potencia estadística es insuficiente para conclusiones definitivas.", # nolint: line_length_linter.
        warn_n_low  = "NOTA: n < 10. Tamaño de muestra limitado. Se recomienda precaución al interpretar la significancia.", # nolint: line_length_linter.
        warn_unbalanced = "ADVERTENCIA: Desequilibrio de grupos detectado (ratio > 1.5). Esto puede afectar la robustez de las pruebas de varianza.", # nolint: line_length_linter.
        info_tlc = "INFO: Se aplicó el Teorema del Límite Central (n >= 30). Se asume normalidad independientemente del valor p de la prueba de Shapiro-Wilk.", # nolint: line_length_linter.
        info_welch = "INFO: Se aplicó corrección de Welch debido a la heterocedasticidad (varianzas desiguales) detectada.", # nolint: line_length_linter.
        msg_robust = "Los datos experimentales cumplen con los criterios estándar de robustez.", # nolint: line_length_linter.
        tit_sup = "VALIDACIÓN DE SUPUESTOS",
        p_lab = "p =",
        homo_lab = "Valor p =",
        nota_label = "Nota:",
        tit_test = "PRUEBA ESTADÍSTICA",
        niveles_txt = "niveles",
        p_global = "Valor p (Global)",
        ph_test_lab = "Prueba Post-hoc:",
        def_resp = "Variable Respuesta",
        msg_wide_auto = "Modo Automático: Se compararán todas las columnas numéricas detectadas.", # nolint: line_length_linter.
        def_x_groups = "Grupos",
        def_y_values = "Valores",
        dt_search = "Buscar:",
        dt_length = "Mostrar _MENU_ registros",
        dt_info   = "Mostrando _START_ a _END_ de _TOTAL_ registros",
        dt_prev   = "Anterior",
        dt_next   = "Siguiente",
        btn_down_fig = "Descargar Figura (.tiff)",
        btn_down_rep = "Descargar Reporte (.txt)",
        footer_text  = "Desarrollado en R & Shiny (tidyverse, rstatix).",
        footer_link  = "Ver metodología y referencias",
        copyright    = paste0("© ", current_year, " BQ. C. Díaz. Todos los derechos reservados."), # nolint: line_length_linter.
        darkmode_switch = "Activar/Desactivar modo oscuro",
        footer_warning = "Por favor, no subir datos médicos sensibles o personales. Herramienta solo para fines de investigación.", # nolint: line_length_linter.
        footer_support_msg = "Si te resultó útil esta herramienta, podrías considerar 'comprarme un café' para mantener su desarrollo y mejora continua.", # nolint: line_length_linter.
        soporte_cl = " En Chile",
        soporte_global = " Resto del Mundo",
        mp_msg = "Apóyame vía:",
        kofi_msg = "Apóyame en:"
      )
    } else {
      list(
        app_title = "EZ Biostats - Smart Biostatistical Analysis Software",
        ver = "Beta Version - Made by Biochem. C. Díaz",
        step1 = "Upload Dataset (Excel, CSV, txt)",
        browse = "Browse...",
        placeholder = "No file selected",
        reset = "Reset",
        processing = "Processing...",
        run = "Run Analysis",
        v_x = "Factor (X Variable):",
        v_y = "Response Variable (Y Variable):",
        cust_title = "Customize Labels (Plot):",
        lab_x = "X-Axis Label:",
        lab_y = "Y-Axis Label:",
        tab_data = "Data Preview",
        tab_plot = "Final Plot",
        tab_rep = "Analysis Report",
        err_num = "Error: Selected Response Variable is not numeric.",
        err_factor = "Error: Factor must have at least 2 groups.",
        err_file = "Please upload a valid file.",
        err_n_small = "Error: Insufficient sample size (n < 3).",
        tit_rep = "BIOSTATISTICAL ANALYSIS REPORT",
        norm = "Normality (Shapiro-Wilk)",
        homo = "Homocedasticity (Levene/Brown-Forsythe)",
        sup_ok = "Assumptions met. Parametric test used.",
        sup_fail = "Assumptions not met. Non-Parametric test used.",
        crit_dec = "If p > 0.05, the assumption is met.",
        var_ana = "Analyzed Variable",
        grupos  = "Compared groups",
        prueba  = "Applied test",
        pval    = "P-value (exact)",
        sig     = "Significance",
        interp  = "Interpretation",
        pos     = "There is sufficient evidence to reject the null hypothesis (p < 0.05). Statistically significant differences were detected.", # nolint: line_length_linter.
        neg     = "There is not sufficient evidence to reject the null hypothesis (p >= 0.05). No significant differences were detected.", # nolint: line_length_linter.
        msd     = "(Mean ± SD)",
        med_iqr = "(Median [IQR])",
        wide_toggle = "My data is in Wide Format.",
        wide_help   = "Wide Format: This is when the column headers are the group names. Example: Column A for 'Control', Column B for 'Treatment', etc.", # nolint: line_length_linter.
        out_check = "Filter Outliers (IQR Method)",
        out_rep = "OUTLIERS DETECTION (TUKEY)",
        out_method = "Criterion: Interquartile Range (1.5 x IQR)",
        out_count = "Outliers detected",
        out_rem = "Decision: Filter. Boxplot is retained to ensure data traceability and visualize excluded data points.", # nolint: line_length_linter.
        out_keep = "Decision: Keep. Boxplot is used due to detected dispersion.", # nolint: line_length_linter.
        out_clean = "Status: Consistent data. Bar chart is justified by normality and lack of outliers.", # nolint: line_length_linter.
        out_mask_tit = "Masking Effect detected:",
        out_mask_msg = "After initial filtering, variance reduction revealed %d additional outlier(s) in the cleaned sample. These values were retained in the statistical analysis.", # nolint: line_length_linter.
        pair_comp = "PAIRWISE COMPARISONS (POST-HOC)",
        just = "Criteria",
        test_t = "Student's t-test",
        test_w = "Welch's t-test",
        test_wilcox = "Wilcoxon rank-sum test",
        test_anova = "One-way ANOVA",
        test_wanova = "Welch's ANOVA",
        test_kw = "Kruskal-Wallis test",
        ph_tukey = "Tukey HSD test",
        ph_gh = "Games-Howell test",
        ph_dunn = "Dunn's test",
        reason_n = "Small sample size (n < 5).",
        reason_norm = "Non-normal distribution.",
        reason_ok = "Assumptions met.",
        grupos_label = "Comparison:",
        prueba_label = "Statistical Method:",
        just_label   = "Justification:",
        priv_note = "Privacy: Data processed in-memory (RAM) only. No files or data is stored.", # nolint: line_length_linter.
        tit_obs = "METHODICAL OBSERVATIONS",
        warn_n_crit = "CRITICAL: n < 5. Results are exploratory; statistical power is insufficient for definitive conclusions.", # nolint: line_length_linter.
        warn_n_low  = "NOTE: n < 10. Limited sample size. Use caution when interpreting significance.", # nolint: line_length_linter.
        warn_unbalanced = "WARNING: Unbalanced groups detected (ratio > 1.5). This may affect the robustness of variance tests.", # nolint: line_length_linter.
        info_tlc = "INFO: Central Limit Theorem applied (n >= 30). Normality is assumed regardless of the p-value from Shapiro-Wilk test.", # nolint: line_length_linter.
        info_welch = "INFO: Welch's correction applied due to detected heteroscedasticity (unequal variances).", # nolint: line_length_linter.
        msg_robust = "Experimental data meets standard robustness criteria.",
        tit_sup = "ASSUMPTION CHECKING",
        p_lab = "p-value:",
        homo_lab = "p-value =",
        nota_label = "Note:",
        tit_test = "STATISTICAL TEST",
        niveles_txt = "levels",
        p_global = "p-value (Global)",
        ph_test_lab = "Post-hoc Test:",
        def_resp = "Response Variable",
        msg_wide_auto = "Automatic Mode: All detected numerical columns will be compared.", # nolint: line_length_linter.
        def_x_groups = "Groups",
        def_y_values = "Values",
        dt_search = "Search:",
        dt_length = "Show _MENU_ entries",
        dt_info   = "Showing _START_ to _END_ of _TOTAL_ entries",
        dt_prev   = "Previous",
        dt_next   = "Next",
        btn_down_fig = "Download Figure (.tiff)",
        btn_down_rep = "Download Report (.txt)",
        footer_text = "Powered by R & Shiny (tidyverse, rstatix).",
        footer_link  = "View methodology and references",
        copyright    = paste0("© ", current_year, " Biochem. C. Díaz. All rights reserved."), # nolint: line_length_linter.
        darkmode_switch = "Toggle dark mode", # nolint: line_length_linter.
        footer_warning = "Please do not upload sensitive or personal medical data. For research purposes only.", # nolint: line_length_linter.
        footer_support_msg = "If you found this tool useful, you may consider 'buying me a coffee' to maintain its development and continuous improvement.", # nolint: line_length_linter.
        soporte_cl = " In Chile",
        soporte_global = " Worldwide",
        mp_msg = "Support me through:",
        kofi_msg = "Support me on:"
      )
    }
  })

  output$darkmode_switch_ui <- renderUI({
    # Usar el diccionario reactivo directamente
    tooltip_text <- lang()$darkmode_switch

    tags$div(
      class = "darkmode-container", # Clase opcional para CSS personalizado
      title = tooltip_text,
      input_dark_mode(id = "dark_mode", mode = "light")
    )
  })

  output$ui_privacy_text <- renderUI({
    lang()$priv_note
  })

  output$ui_footer <- renderUI({
    l <- lang()

    # SVG Real de Mercado Pago y Ko-fi
    # nolint start
    mp_svg <- HTML('
      <?xml version="1.0" encoding="UTF-8"?>
      <svg id="logos" xmlns="http://www.w3.org/2000/svg" viewBox="100 80 848 265" width="120" height="40">
        <defs>
          <style>
            .cls-1 {
              fill: #0a0080;
            }

            .cls-1, .cls-2, .cls-3 {
              stroke-width: 0px;
            }

            .cls-2 {
              fill: #fff;
            }

            .cls-3 {
              fill: #00bcff;
            }
          </style>
        </defs>
        <path class="cls-3" d="m274.38,116.94c-77.83,0-140.91,40.36-140.91,90.15s63.09,94.05,140.91,94.05,140.91-44.27,140.91-94.05-63.09-90.15-140.91-90.15Z"/>
        <path class="cls-2" d="m228.53,179.22c-.07.14-1.45,1.56-.55,2.71,2.18,2.78,8.91,4.38,15.72,2.85,4.05-.91,9.25-5.04,14.28-9.03,5.45-4.33,10.86-8.67,16.3-10.39,5.76-1.83,9.45-1.05,11.89-.31,2.67.8,5.82,2.56,10.84,6.32,9.45,7.1,47.43,40.26,54,45.99,5.28-2.39,30.47-12.56,62.39-19.6-2.78-17.02-13.01-33.25-28.72-45.99-21.89,9.19-50.42,14.7-76.58,1.93-.13-.05-14.29-6.75-28.25-6.42-20.75.48-29.74,9.46-39.25,18.97l-12.05,12.99Z"/>
        <path class="cls-2" d="m349.44,220.97c-.45-.4-44.67-39.09-54.69-46.62-5.8-4.35-9.02-5.46-12.41-5.89-1.76-.23-4.2.1-5.9.57-4.66,1.27-10.75,5.34-16.16,9.63-5.6,4.46-10.88,8.66-15.79,9.76-6.26,1.4-13.91-.25-17.4-2.61-1.41-.95-2.41-2.05-2.89-3.16-1.29-2.99,1.09-5.38,1.48-5.78l12.2-13.2c1.42-1.41,2.85-2.83,4.31-4.23-3.94.51-7.58,1.52-11.12,2.5-4.42,1.24-8.68,2.42-12.98,2.42-1.8,0-11.42-1.58-13.25-2.07-11.05-3.02-23.56-5.97-38.04-12.73-17.35,12.91-28.65,28.77-32,46.56,2.49.66,9.02,2.15,10.71,2.52,39.26,8.73,51.49,17.72,53.71,19.6,2.4-2.67,5.87-4.36,9.73-4.36,4.35,0,8.26,2.19,10.64,5.56,2.25-1.78,5.35-3.3,9.36-3.29,1.82,0,3.71.34,5.62.98,4.43,1.52,6.72,4.47,7.9,7.14,1.48-.67,3.31-1.17,5.46-1.16,2.12,0,4.32.48,6.53,1.44,7.24,3.11,8.36,10.22,7.71,15.58.52-.06,1.04-.08,1.56-.08,8.58,0,15.56,6.98,15.56,15.57,0,2.66-.68,5.16-1.86,7.35,2.34,1.31,8.29,4.28,13.52,3.62,4.17-.53,5.76-1.95,6.32-2.76.39-.55.8-1.2.42-1.66l-11.08-12.3s-1.82-1.73-1.22-2.39c.62-.68,1.75.3,2.55.96,5.64,4.71,12.52,11.81,12.52,11.81.12.08.57.98,3.12,1.43,2.19.39,6.07.17,8.76-2.04.67-.56,1.35-1.25,1.93-1.97-.05.04-.09.08-.13.1,2.84-3.63-.32-7.29-.32-7.29l-12.93-14.52s-1.85-1.71-1.22-2.4c.56-.6,1.75.3,2.56.98,4.09,3.42,9.88,9.23,15.42,14.66,1.09.79,5.96,3.8,12.41-.43,3.92-2.57,4.7-5.73,4.59-8.1-.27-3.15-2.73-5.4-2.73-5.4l-17.66-17.76s-1.87-1.59-1.21-2.4c.54-.68,1.75.3,2.55.96,5.62,4.71,20.86,18.68,20.86,18.68.22.15,5.48,3.9,11.99-.24,2.33-1.49,3.81-3.73,3.94-6.34.22-4.52-2.96-7.2-2.96-7.2Z"/>
        <path class="cls-2" d="m263.76,243.48c-2.74-.03-5.74,1.6-6.13,1.36-.22-.14.17-1.24.42-1.88.27-.63,3.87-11.48-4.92-15.25-6.73-2.89-10.85.36-12.26,1.83-.37.38-.54.35-.58-.13-.14-1.96-1.01-7.24-6.82-9.02-8.3-2.54-13.64,3.25-14.99,5.35-.61-4.73-4.61-8.4-9.5-8.41-5.32,0-9.64,4.3-9.65,9.63,0,5.32,4.31,9.64,9.64,9.64,2.59,0,4.93-1.03,6.66-2.69.06.05.08.14.05.32-.41,2.39-1.15,11.04,7.92,14.57,3.64,1.41,6.73.36,9.29-1.43.76-.54.89-.31.78.41-.33,2.23.09,6.99,6.77,9.7,5.08,2.07,8.09-.04,10.07-1.87.86-.78,1.09-.65,1.14.56.24,6.44,5.59,11.56,12.09,11.57,6.7,0,12.13-5.41,12.13-12.1,0-6.7-5.42-12.06-12.12-12.13Z"/>
        <path class="cls-1" d="m274.35,113.21c-79.31,0-143.6,42.18-143.6,93.92,0,1.34-.02,5.03-.02,5.5,0,54.9,56.19,99.35,143.6,99.35s143.61-44.45,143.61-99.34v-5.51c0-51.74-64.29-93.92-143.59-93.92Zm137.12,83.51c-31.21,6.94-54.49,17.01-60.32,19.61-13.62-11.89-45.1-39.26-53.63-45.66-4.87-3.67-8.2-5.6-11.12-6.47-1.31-.4-3.12-.85-5.45-.85-2.17,0-4.5.39-6.93,1.17-5.51,1.75-11,6.11-16.31,10.33l-.27.22c-4.95,3.93-10.06,8-13.93,8.86-1.69.38-3.43.58-5.16.58-4.34,0-8.23-1.26-9.69-3.12-.24-.31-.08-.81.48-1.52l.07-.1,11.99-12.91c9.39-9.39,18.25-18.25,38.66-18.72.34-.01.68-.02,1.02-.02,12.7.01,25.4,5.69,26.83,6.36,11.91,5.81,24.21,8.76,36.56,8.77,12.85,0,26.11-3.17,40.05-9.58,14.56,12.24,24.21,26.99,27.15,43.06Zm-137.1-77.97c42.1,0,79.76,12.07,105.09,31.07-12.24,5.3-23.91,7.97-35.17,7.97-11.52-.01-23.03-2.78-34.21-8.23-.59-.28-14.61-6.89-29.2-6.9-.38,0-.77,0-1.15.01-17.14.4-26.8,6.49-33.29,11.82-6.31.16-11.76,1.68-16.61,3.03-4.33,1.2-8.06,2.24-11.7,2.24-1.5,0-4.2-.14-4.44-.15-4.18-.13-25.18-5.28-41.95-11.61,25.27-17.96,61.89-29.26,102.64-29.26Zm-107.61,33.01c17.51,7.16,38.76,12.7,45.48,13.13,1.87.12,3.87.34,5.87.34,4.46,0,8.91-1.25,13.21-2.45,2.54-.71,5.35-1.49,8.3-2.05-.79.77-1.58,1.56-2.37,2.35l-12.17,13.17c-.96.97-3.04,3.55-1.67,6.73.54,1.28,1.65,2.51,3.2,3.55,2.9,1.95,8.1,3.28,12.92,3.28,1.83,0,3.57-.18,5.15-.54,5.11-1.14,10.46-5.41,16.13-9.92,4.52-3.59,10.94-8.15,15.86-9.49,1.38-.37,3.06-.61,4.42-.61.41,0,.79.02,1.14.07,3.24.41,6.38,1.51,11.99,5.72,10,7.51,54.22,46.2,54.65,46.58.03.02,2.85,2.46,2.65,6.5-.11,2.26-1.36,4.26-3.54,5.65-1.89,1.2-3.83,1.81-5.8,1.81-2.96,0-4.99-1.39-5.13-1.48-.16-.13-15.31-14.03-20.89-18.7-.89-.74-1.75-1.4-2.62-1.4-.47,0-.88.2-1.16.55-.88,1.08.1,2.58,1.26,3.56l17.7,17.8s2.21,2.06,2.45,4.79c.14,2.95-1.27,5.42-4.2,7.34-2.09,1.38-4.2,2.07-6.27,2.07-2.72,0-4.63-1.24-5.05-1.53l-2.54-2.5c-4.64-4.57-9.43-9.29-12.94-12.21-.86-.71-1.77-1.37-2.64-1.37-.43,0-.82.16-1.12.48-.4.44-.68,1.24.32,2.57.4.55.89,1,.89,1l12.91,14.51c.1.13,2.66,3.17.29,6.19l-.46.58c-.39.42-.8.82-1.2,1.16-2.2,1.81-5.14,2-6.31,2-.63,0-1.22-.05-1.75-.15-1.27-.23-2.13-.58-2.55-1.07l-.16-.16c-.7-.73-7.21-7.38-12.6-11.87-.71-.6-1.6-1.34-2.51-1.34-.45,0-.85.18-1.17.52-1.06,1.17.54,2.91,1.22,3.55l11.01,12.15c-.01.11-.15.36-.41.74-.4.55-1.73,1.88-5.73,2.38-.48.06-.98.09-1.46.09-4.12,0-8.52-2-10.79-3.2,1.03-2.18,1.57-4.58,1.57-6.98,0-9.07-7.36-16.44-16.43-16.45-.19,0-.4,0-.59.01.29-4.14-.29-11.98-8.34-15.43-2.32-1-4.63-1.52-6.87-1.52-1.76,0-3.45.3-5.04.91-1.67-3.24-4.44-5.6-8.04-6.83-2-.69-3.98-1.04-5.9-1.04-3.35,0-6.44.99-9.19,2.94-2.64-3.28-6.62-5.22-10.81-5.22-3.67,0-7.2,1.47-9.81,4.06-3.43-2.62-17.03-11.26-53.44-19.53-1.74-.39-5.69-1.52-8.17-2.25,3.41-16.34,13.8-31.27,29.2-43.52Zm67.54,94.78l-.39-.35h-.4c-.32,0-.66.13-1.11.45-1.86,1.31-3.63,1.94-5.44,1.94-1,0-2.02-.2-3.04-.59-8.44-3.29-7.78-11.25-7.36-13.65.06-.49-.06-.86-.37-1.12l-.6-.49-.56.53c-1.65,1.59-3.8,2.45-6.06,2.45-4.83,0-8.77-3.93-8.76-8.77,0-4.83,3.94-8.76,8.78-8.75,4.37,0,8.09,3.28,8.64,7.65l.3,2.35,1.29-1.99c.14-.23,3.69-5.59,10.2-5.58,1.24,0,2.52.2,3.81.6,5.19,1.58,6.07,6.29,6.2,8.25.09,1.14.91,1.2,1.06,1.2.45,0,.78-.28,1.01-.53.98-1.02,3.11-2.72,6.45-2.72,1.53,0,3.15.37,4.83,1.09,8.25,3.54,4.51,14.02,4.47,14.13-.71,1.74-.74,2.5-.07,2.95l.32.15h.24c.37,0,.83-.16,1.6-.42,1.12-.39,2.81-.97,4.4-.97h0c6.21.07,11.26,5.13,11.26,11.26,0,6.2-5.06,11.24-11.27,11.24-6.07,0-11.01-4.73-11.23-10.74-.02-.52-.07-1.88-1.23-1.88-.47,0-.89.29-1.36.72-1.34,1.24-3.04,2.49-5.52,2.49-1.13,0-2.35-.26-3.64-.79-6.41-2.6-6.5-7-6.24-8.77.07-.47.09-.96-.23-1.35Zm40.07,48.88c-76.26,0-138.08-39.55-138.08-88.33,0-1.96.14-3.91.33-5.84.61.15,6.67,1.59,7.92,1.88,37.19,8.26,49.48,16.85,51.56,18.48-.7,1.69-1.07,3.51-1.07,5.35,0,7.69,6.25,13.95,13.93,13.95.86,0,1.72-.08,2.56-.24,1.16,5.66,4.86,9.95,10.51,12.15,1.65.63,3.32.96,4.97.96,1.06,0,2.13-.13,3.17-.39,1.05,2.65,3.39,5.96,8.65,8.09,1.84.74,3.68,1.13,5.47,1.13,1.46,0,2.89-.26,4.25-.76,2.52,6.13,8.51,10.2,15.19,10.2,4.43,0,8.68-1.8,11.78-4.99,2.65,1.48,8.25,4.15,13.91,4.16.73,0,1.41-.05,2.11-.13,5.62-.71,8.23-2.91,9.43-4.62.22-.3.41-.62.58-.95,1.32.38,2.78.69,4.46.7,3.07,0,6.01-1.05,8.99-3.21,2.93-2.11,5.01-5.14,5.31-7.72,0-.03,0-.07.01-.11.99.2,2,.3,3.01.3,3.16,0,6.27-.98,9.24-2.93,5.73-3.75,6.72-8.66,6.63-11.87,1.01.21,2.03.32,3.05.32,2.96,0,5.88-.89,8.65-2.66,3.55-2.27,5.69-5.75,6.02-9.79.21-2.75-.47-5.53-1.91-7.91,9.58-4.13,31.48-12.12,57.27-17.93.11,1.46.17,2.93.17,4.41,0,48.78-61.82,88.33-138.07,88.33Z"/>
        <g>
          <path class="cls-1" d="m910.26,142.12c-5.21-6.54-13.13-9.8-23.75-9.8s-18.53,3.27-23.74,9.8c-5.22,6.53-7.83,14.25-7.83,23.16s2.61,16.81,7.83,23.26c5.21,6.43,13.13,9.65,23.74,9.65s18.54-3.22,23.75-9.65c5.22-6.45,7.82-14.19,7.82-23.26s-2.6-16.63-7.82-23.16Zm-12.92,37.48c-2.53,3.35-6.15,5.04-10.89,5.04s-8.36-1.69-10.91-5.04c-2.55-3.35-3.82-8.13-3.82-14.32s1.27-10.95,3.82-14.29c2.55-3.34,6.19-5.01,10.91-5.01s8.35,1.67,10.89,5.01c2.53,3.34,3.8,8.11,3.8,14.29s-1.27,10.97-3.8,14.32Z"/>
          <path class="cls-1" d="m776.98,136.65c-5.29-2.68-11.34-4.03-18.15-4.03-10.47,0-17.86,2.73-22.17,8.18-2.71,3.49-4.22,7.95-4.58,13.37h15.65c.38-2.4,1.15-4.29,2.31-5.69,1.61-1.89,4.36-2.84,8.23-2.84,3.46,0,6.08.48,7.88,1.45,1.78.96,2.68,2.72,2.68,5.26,0,2.09-1.16,3.61-3.49,4.61-1.3.57-3.46,1.04-6.48,1.42l-5.55.68c-6.3.8-11.08,2.13-14.32,3.99-5.92,3.41-8.88,8.93-8.88,16.55,0,5.87,1.83,10.41,5.52,13.61,3.67,3.21,8.34,4.55,13.98,4.81,35.37,1.59,34.98-18.64,35.3-22.84v-23.27c0-7.47-2.65-12.55-7.93-15.25Zm-8.22,35.32c-.11,5.42-1.66,9.15-4.64,11.2-2.99,2.05-6.24,3.07-9.78,3.07-2.24,0-4.14-.63-5.7-1.85-1.56-1.23-2.34-3.24-2.34-6.01,0-3.1,1.28-5.39,3.83-6.88,1.51-.87,3.99-1.61,7.45-2.2l3.69-.69c1.84-.35,3.28-.73,4.34-1.13,1.07-.38,2.1-.9,3.13-1.55v6.03Z"/>
          <path class="cls-1" d="m696.32,146.48c4.05,0,7.01,1.25,8.94,3.75,1.31,1.84,2.13,3.93,2.45,6.24h17.45c-.95-8.81-4.03-14.95-9.24-18.43-5.22-3.47-11.9-5.21-20.07-5.21-9.61,0-17.15,2.95-22.61,8.84-5.46,5.9-8.2,14.15-8.2,24.75,0,9.38,2.47,17.04,7.42,22.93,4.95,5.89,12.66,8.84,23.14,8.84s18.42-3.53,23.76-10.61c3.35-4.38,5.23-9.03,5.62-13.94h-17.39c-.36,3.25-1.37,5.9-3.06,7.94-1.67,2.03-4.5,3.06-8.5,3.06-5.63,0-9.47-2.57-11.5-7.72-1.12-2.75-1.69-6.38-1.69-10.91s.57-8.54,1.69-11.43c2.12-5.39,6.05-8.1,11.79-8.1Z"/>
          <path class="cls-1" d="m660.36,132.83c-35.85,0-33.72,31.73-33.72,31.73v32.24h16.27v-30.23c0-4.96.63-8.62,1.86-11.01,2.23-4.23,6.6-6.35,13.1-6.35.49,0,1.13.03,1.92.07.79.04,1.69.11,2.73.23v-16.55c-.72-.05-1.19-.07-1.39-.1-.21-.02-.46-.03-.77-.03Z"/>
          <path class="cls-1" d="m613.6,144.85c-2.81-4.16-6.38-7.21-10.68-9.15-4.31-1.92-9.15-2.88-14.52-2.88-9.06,0-16.42,2.85-22.1,8.56-5.67,5.72-8.52,13.92-8.52,24.63,0,11.43,3.15,19.67,9.44,24.74,6.28,5.06,13.54,7.61,21.76,7.61,9.96,0,17.71-3.01,23.24-9.02,2.99-3.16,4.86-6.29,5.65-9.38h-17.26c-.68.98-1.41,1.81-2.22,2.46-2.3,1.89-5.42,2.47-9.09,2.47-3.47,0-6.2-.52-8.66-2.07-4.06-2.5-6.35-6.72-6.59-12.91h45.01c.06-5.34-.11-9.43-.54-12.27-.74-4.84-2.4-9.1-4.92-12.77Zm-39.15,14.38c.58-4.02,2.03-7.2,4.3-9.56,2.29-2.35,5.5-3.53,9.65-3.53,3.81,0,7.01,1.11,9.59,3.34,2.57,2.22,4,5.48,4.3,9.75h-27.83Z"/>
          <path class="cls-1" d="m525.46,132.61c-7.55,0-14.08,3.31-18.47,8.61-4.17-5.3-10.59-8.61-18.48-8.61-15.89,0-26.13,11.67-26.13,27.12v37.06h14.87v-37.41c0-6.83,4.62-11.55,11.27-11.55,9.8,0,10.81,8.13,10.81,11.55v37.41h14.87v-37.41c0-6.83,4.73-11.55,11.26-11.55,9.8,0,10.93,8.13,10.93,11.55v37.41h14.85v-37.06c0-15.93-9.56-27.12-25.79-27.12Z"/>
          <path class="cls-1" d="m833.71,124.7l-.02,17.43c-1.81-2.92-4.17-5.2-7.08-6.83-2.9-1.64-6.23-2.47-9.98-2.47-8.13,0-14.6,3.03-19.46,9.06-4.86,6.05-7.29,14.77-7.29,25.31,0,9.15,2.47,16.65,7.4,22.49,4.93,5.83,14.6,8.39,23.19,8.39,29.95,0,29.6-25.68,29.6-25.68v-59.11s-16.37-1.75-16.37,11.41Zm-3.13,55.04c-2.37,3.4-5.86,5.1-10.43,5.1s-7.98-1.72-10.23-5.13c-2.25-3.43-3.37-8.41-3.37-14.11,0-5.3,1.1-9.72,3.31-13.29,2.21-3.57,5.67-5.36,10.4-5.36,3.1,0,5.82.98,8.17,2.94,3.81,3.25,5.73,9.09,5.73,16.64,0,5.4-1.2,9.81-3.58,13.21Z"/>
        </g>
        <path class="cls-1" d="m496.75,221.66c-13.4-.63-20.16,2.56-24.57,5.93-6.09,4.65-9.8,11.53-9.8,22.52v56.51h7.88c2.11,0,4.22-.73,5.77-2.16,1.74-1.6,2.61-3.56,2.61-5.86v-21.12c1.92,3.31,4.45,5.74,7.65,7.32,3.03,1.41,6.53,2.12,10.51,2.12,7.49,0,13.64-2.98,18.41-8.97,4.78-6.15,7.17-14.15,7.17-24.06s-2.26-16.97-7.68-23.57c-4.38-5.34-11.04-8.35-17.94-8.66Zm5.55,46.38c-2.39,3.31-5.66,4.96-9.8,4.96-4.46,0-7.89-1.64-10.28-4.96-2.39-2.99-3.59-7.45-3.59-13.45,0-6.43,1.11-11.16,3.34-14.15,2.4-3.29,5.75-4.96,10.05-4.96s7.89,1.66,10.28,4.96c2.4,3.31,3.59,8.02,3.59,14.15,0,5.68-1.19,10.14-3.59,13.45Z"/>
        <path class="cls-1" d="m636.47,227.49c-5.53-4.19-11.18-6.38-20.89-6.12-9.86.27-17.03,3.03-21.49,9.07-4.46,6.05-6.68,13.95-6.68,23.68,0,8.33,1.68,15.04,5.04,20.17,3.37,5.1,7.4,8.6,12.1,10.47,4.68,1.89,9.42,2.28,14.2,1.19,4.77-1.11,8.57-3.84,11.39-8.24v3.99c-.32,5.03-1.53,8.8-3.63,11.32-2.13,2.5-4.47,4.04-7.06,4.59-2.56.54-5.16.24-7.73-.95-2.59-1.17-4.5-2.87-5.75-5.06h-17.14c4.44,13.34,12.41,19.23,26.77,20.27,23.16,1.67,30.54-17.94,30.52-28.52v-33.25c0-10.99-3.58-18.03-9.63-22.63Zm-6.81,32.66c-.63,3.68-1.64,6.4-3.06,8.12-2.97,4.08-7.6,5.53-13.84,4.37-6.27-1.19-9.4-7.2-9.4-18.03,0-5.03.93-9.51,2.82-13.45,1.88-3.91,5.47-5.89,10.79-5.89,3.91,0,6.89,1.42,8.92,4.24,2.04,2.83,3.34,6.05,3.88,9.67.55,3.61.5,7.27-.12,10.96Z"/>
        <path class="cls-1" d="m573.49,225.84c-5.29-2.67-11.34-4.03-18.15-4.03-10.47,0-17.85,2.73-22.15,8.19-2.7,3.48-4.22,7.94-4.58,13.36h15.65c.38-2.39,1.15-4.29,2.3-5.68,1.61-1.89,4.36-2.85,8.23-2.85,3.47,0,6.09.48,7.88,1.45,1.78.96,2.67,2.72,2.67,5.26,0,2.08-1.16,3.62-3.49,4.6-1.3.57-3.46,1.04-6.48,1.42l-5.54.67c-6.3.8-11.09,2.13-14.31,3.99-5.93,3.41-8.88,8.92-8.88,16.54,0,5.87,1.83,10.41,5.52,13.61,3.67,3.21,8.34,4.55,13.99,4.81,35.36,1.58,34.96-18.64,35.28-22.84v-23.27c0-7.46-2.63-12.54-7.92-15.24Zm-8.22,35.31c-.1,5.43-1.66,9.15-4.63,11.2-2.98,2.05-6.24,3.07-9.78,3.07-2.24,0-4.13-.63-5.7-1.85-1.56-1.23-2.34-3.23-2.34-6,0-3.1,1.28-5.39,3.83-6.87,1.52-.87,3.99-1.61,7.45-2.2l3.7-.68c1.84-.35,3.29-.72,4.33-1.12,1.07-.39,2.11-.91,3.14-1.56v6.03Z"/>
        <path class="cls-1" d="m707.61,230.97c-5.22-6.54-13.14-9.81-23.76-9.81s-18.52,3.26-23.73,9.81c-5.22,6.53-7.83,14.24-7.83,23.15s2.61,16.8,7.83,23.25c5.21,6.42,13.13,9.64,23.73,9.64s18.53-3.22,23.76-9.64c5.21-6.45,7.81-14.19,7.81-23.25s-2.6-16.62-7.81-23.15Zm-12.93,37.46c-2.53,3.36-6.15,5.05-10.87,5.05s-8.36-1.69-10.91-5.05c-2.56-3.35-3.83-8.12-3.83-14.31s1.27-10.95,3.83-14.29c2.54-3.34,6.18-5.01,10.91-5.01s8.35,1.67,10.87,5.01c2.53,3.34,3.79,8.1,3.79,14.29s-1.26,10.96-3.79,14.31Z"/>
      </svg>
    ')

    kofi_svg <- HTML('
      <svg viewBox="0 0 739 201" width="120" height="32" fill="none" xmlns="http://www.w3.org/2000/svg">
      <g clip-path="url(#clip0_1_194)">
      <mask id="mask0_1_194" style="mask-type:luminance" maskUnits="userSpaceOnUse" x="-1" y="0" width="739" height="201">
      <path d="M737.178 0.299805H-0.00585938V200.865H737.178V0.299805Z" fill="white"/>
      </mask>
      <g mask="url(#mask0_1_194)">
      <path d="M345.948 199.597C333.351 199.597 320.101 192.881 308.601 181.463C308.164 182.224 307.701 182.96 307.225 183.671C302.379 190.933 293.043 199.597 276.038 199.597C264.399 199.597 248.713 195.081 240.055 173.567C234.251 159.149 231.549 137.051 231.549 104.018C231.549 75.0379 234.892 51.931 241.482 35.3447C245.294 25.7418 250.299 18.1431 256.35 12.7581C263.454 6.44068 272.112 3.09801 281.378 3.09801C293.607 3.09801 304.098 8.61626 310.917 18.6378C311.957 20.1728 312.908 21.8029 313.764 23.5281C327.326 9.94188 341.121 3.09167 354.999 3.09167C373.279 3.09167 385.084 15.7202 385.084 35.2686C385.084 46.305 379.97 59.1174 369.879 73.3382C364.063 81.5328 356.826 89.9244 348.593 98.0242C363.72 120.871 377.649 149.261 377.649 167.179C377.649 175.844 374.402 183.963 368.496 190.039C362.515 196.198 354.504 199.591 345.935 199.591L345.948 199.597Z" fill="white"/>
      <path d="M369.867 35.2693C369.867 25.9772 366.15 18.3151 355.006 18.3151C342.929 18.3151 325.049 26.9096 303.217 62.2069C303.914 58.0273 304.143 54.0757 304.143 50.3652C304.143 31.3178 296.246 18.3151 281.385 18.3151C258.855 18.3151 246.779 50.1306 246.779 104.013C246.779 167.187 256.762 184.369 276.044 184.369C292.301 184.369 300.66 173.218 301.593 144.657C313.207 169.971 332.019 184.369 345.954 184.369C355.012 184.369 362.446 176.935 362.446 167.18C362.446 152.782 347.813 121.893 328.537 95.8809C350.604 76.6051 369.879 51.7543 369.879 35.263L369.867 35.2693Z" fill="white"/>
      <path d="M695.015 83.1239C672.151 83.1239 653.549 64.2412 653.549 41.0269C653.549 29.9206 657.78 19.6326 665.451 12.0593C673.142 4.47334 683.639 0.299805 695.015 0.299805C706.396 0.299805 717.069 4.44163 724.879 11.9578C732.811 19.5946 737.177 29.9967 737.177 41.2553C737.177 64.343 718.264 83.1239 695.015 83.1239Z" fill="white"/>
      <path d="M695.016 67.9021C709.879 67.9021 721.954 55.8315 721.954 41.256C721.954 26.6802 710.578 15.5232 695.016 15.5232C679.458 15.5232 668.769 26.6802 668.769 41.0275C668.769 55.375 680.384 67.9021 695.016 67.9021Z" fill="white"/>
      <path d="M691.999 200.862C677.025 200.862 664.762 193.416 657.486 179.906C651.558 168.888 648.67 153.627 648.67 133.261C648.67 112.894 651.569 97.6587 657.532 86.6412C664.935 72.9599 677.505 65.4249 692.924 65.4249C708.344 65.4249 719.501 72.0784 726.574 84.6623C732.54 95.2801 735.32 110.141 735.32 131.434C735.32 152.727 732.498 168.349 726.689 179.493C719.509 193.27 707.184 200.862 691.999 200.862Z" fill="white"/>
      <path d="M692.924 80.6542C672.951 80.6542 663.894 98.4203 663.894 133.261C663.894 168.102 673.185 185.64 691.999 185.64C710.812 185.64 720.104 168.559 720.104 131.441C720.104 94.3228 711.047 80.6542 692.932 80.6542H692.924Z" fill="white"/>
      <path d="M597.708 200.865C582.396 200.865 571.631 193.92 565.725 180.226C561.94 171.447 559.966 159.681 559.275 140.983C554.353 138.896 550.275 136.194 546.95 132.826C540.627 126.42 537.424 118.06 537.424 107.981C537.424 97.2809 540.496 88.5153 546.551 81.9314C549.838 78.3606 553.923 75.4998 558.887 73.3117C559.77 51.7905 566.578 33.7009 578.711 20.787C591.124 7.57497 608.831 0.299805 628.597 0.299805C642.753 0.299805 655.044 4.10548 664.139 11.2982C673.8 18.9413 679.121 29.743 679.121 41.7118C679.121 57.5816 670.782 70.1909 657.317 76.6225C665.706 82.0838 672.152 91.0459 672.152 105.932C672.152 119.328 667.514 129.61 658.369 136.505C653.047 140.513 646.405 143.272 637.616 145.036C636.468 161.248 634.019 172.57 629.96 180.885C623.567 193.958 612.418 200.865 597.708 200.865Z" fill="white"/>
      <path d="M663.892 41.7125C663.892 27.3652 651.118 15.5232 628.588 15.5232C597.934 15.5232 574.01 36.4734 574.01 78.152V84.5264C559.147 87.4885 552.639 94.3198 552.639 107.982C552.639 119.824 559.143 126.655 574.236 129.617C574.701 173.801 580.51 185.643 597.692 185.643C614.873 185.643 621.846 173.116 623.006 131.672C647.624 129.624 656.916 122.792 656.916 105.94C656.916 90.9072 647.624 84.761 623.006 82.9406C622.771 74.7395 622.771 68.8217 622.545 64.4957C626.261 65.4091 631.138 66.0879 635.319 66.0879C652.969 66.0879 663.885 56.2945 663.885 41.719L663.892 41.7125Z" fill="white"/>
      <path d="M418.638 199.597C398.105 199.597 379.287 190.927 365.65 175.184C352.565 160.082 345.353 139.588 345.353 117.49C345.353 95.3918 352.539 75.7036 365.586 61.4767C378.957 46.8947 397.801 38.8586 418.638 38.8586C439.475 38.8586 458.354 46.7807 471.65 61.1661C484.912 75.5197 491.923 94.9921 491.923 117.484C491.923 139.975 484.716 160.075 471.623 175.178C457.982 190.92 439.164 199.591 418.638 199.591V199.597Z" fill="white"/>
      <path d="M418.639 54.082C384.494 54.082 360.576 80.0935 360.576 117.484C360.576 154.875 385.427 184.369 418.639 184.369C451.85 184.369 476.7 155.573 476.7 117.484C476.7 79.3958 452.78 54.082 418.639 54.082ZM416.089 131.654C408.89 131.654 403.55 125.153 403.55 117.256C403.55 110.057 408.89 104.247 416.089 104.247C423.288 104.247 428.399 110.05 428.399 117.256C428.399 125.153 423.058 131.654 416.089 131.654Z" fill="white"/>
      <path d="M511.712 143.855C498.455 143.855 488.921 141.363 481.703 136.009C472.868 129.451 468.384 119.309 468.384 105.875C468.384 90.1383 475.456 81.4614 481.388 76.9516C489.094 71.091 499.395 68.3572 513.797 68.3572C528.203 68.3572 538.846 70.8944 546.621 76.3365C553.106 80.8778 560.839 89.8085 560.839 106.573C560.839 123.337 552.63 132.337 545.742 136.669C537.844 141.642 527.347 143.855 511.705 143.855H511.712Z" fill="white"/>
      <path d="M513.797 83.5813C492.2 83.5813 483.607 90.7803 483.607 105.876C483.607 123.293 493.363 128.634 511.712 128.634C535.632 128.634 545.619 123.059 545.619 106.568C545.619 90.0762 535.636 83.5748 513.805 83.5748L513.797 83.5813Z" fill="white"/>
      <path d="M96.2458 197.935C61.2019 197.935 32.6975 182.263 15.9906 153.803C1.19927 128.856 -0.00585938 101.836 -0.00585938 71.6443C-0.00585938 53.745 5.37915 38.1609 15.5594 26.5726C24.9149 15.9294 38.1713 9.03478 52.8928 7.15734C70.3672 4.94372 92.0914 4.75977 114.678 4.75977C151.434 4.75977 161.817 5.2101 176.279 6.65625C195.517 8.56544 211.704 15.7455 223.082 27.4225C234.639 39.2835 240.747 55.1214 240.747 73.2427V76.8837C240.747 107.805 220.076 133.683 191.267 140.717C189.117 145.792 186.453 150.84 183.301 155.813L183.218 155.94C173.07 171.626 149.214 197.942 103.521 197.942H96.2396L96.2458 197.935Z" fill="white"/>
      <path d="M174.77 21.7977C161.114 20.434 151.555 19.9773 114.684 19.9773C91.0132 19.9773 70.9829 20.2056 54.8278 22.2543C33.4335 24.9881 15.2234 41.3715 15.2234 71.6456C15.2234 101.92 16.8154 125.362 29.1077 146.072C42.9922 169.743 66.2066 182.714 96.2526 182.714H103.534C140.404 182.714 160.435 163.14 170.45 147.664C174.776 140.833 177.96 134.008 180.009 127.177C206.185 124.9 225.531 103.277 225.531 76.8784V73.2378C225.531 44.7902 206.871 24.9881 174.776 21.7977H174.77Z" fill="white"/>
      <path d="M683.057 58.6707H648.67V83.0335H683.057V58.6707Z" fill="white"/>
      <path d="M668.769 41.0275C668.769 55.8315 680.384 67.9021 695.016 67.9021C709.648 67.9021 721.954 55.8315 721.954 41.256C721.954 26.6802 710.578 15.5232 695.016 15.5232C679.458 15.5232 668.769 26.6802 668.769 41.0275ZM663.893 133.264C663.893 168.562 673.185 185.643 691.998 185.643C710.812 185.643 720.103 168.562 720.103 131.444C720.103 94.3259 711.046 80.6573 692.931 80.6573C672.958 80.6573 663.901 98.4234 663.901 133.264M552.647 107.982C552.647 119.824 559.148 126.655 574.245 129.617C574.705 173.801 580.518 185.643 597.7 185.643C614.882 185.643 621.854 173.116 623.014 131.672C647.629 129.624 656.921 122.792 656.921 105.94C656.921 90.9072 647.629 84.761 623.014 82.9406C622.78 74.7395 622.78 68.8217 622.549 64.4957C626.266 65.4091 631.146 66.0879 635.323 66.0879C652.977 66.0879 663.893 56.2945 663.893 41.719C663.893 27.1432 651.119 15.5295 628.589 15.5295C597.934 15.5295 574.01 36.4798 574.01 78.1582V84.5329C559.148 87.4947 552.64 94.3259 552.64 107.988M483.606 105.876C483.606 123.293 493.362 128.634 511.711 128.634C535.631 128.634 545.617 123.059 545.617 106.567C545.617 90.0764 535.631 83.5749 513.803 83.5749C491.98 83.5749 483.613 90.774 483.613 105.87M403.541 117.255C403.541 110.056 408.881 104.246 416.08 104.246C423.279 104.246 428.394 110.05 428.394 117.255C428.394 125.152 423.053 131.653 416.08 131.653C409.108 131.653 403.541 125.152 403.541 117.255ZM360.575 117.49C360.575 155.578 385.426 184.374 418.637 184.374C451.849 184.374 476.698 155.578 476.698 117.49C476.698 79.4014 452.778 54.0872 418.637 54.0872C384.493 54.0872 360.575 80.099 360.575 117.49ZM281.379 18.314C258.849 18.314 246.773 50.1294 246.773 104.011C246.773 167.185 256.762 184.368 276.038 184.368C292.295 184.368 300.655 173.218 301.587 144.656C313.201 169.97 332.013 184.368 345.948 184.368C355.006 184.368 362.439 176.934 362.439 167.179C362.439 152.781 347.807 121.892 328.531 95.8798C350.598 76.6043 369.873 51.7532 369.873 35.2619C369.873 25.9698 366.156 18.3077 355.012 18.3077C342.936 18.3077 325.055 26.9022 303.223 62.1997C303.921 58.02 304.149 54.0684 304.149 50.3579C304.149 31.3104 296.253 18.3077 281.391 18.3077" fill="#202020"/>
      <path d="M15.218 71.6456C15.218 41.3715 33.4281 24.9881 54.8222 22.2543C70.9838 20.2056 91.0144 19.9773 114.679 19.9773C151.55 19.9773 161.108 20.434 174.764 21.7977C206.859 24.9818 225.519 44.7841 225.519 73.2378V76.8784C225.519 103.283 206.174 124.906 179.997 127.177C177.948 134.008 174.764 140.833 170.439 147.664C160.423 163.14 140.393 182.714 103.522 182.714H96.2408C66.1951 182.714 42.9804 169.743 29.096 146.072C16.8037 125.362 15.2117 102.37 15.2117 71.6456" fill="#202020"/>
      <path d="M32.285 71.8719C32.285 101.233 34.1054 120.121 43.6639 137.647C54.5861 157.905 74.3884 165.644 96.9243 165.644H103.977C133.567 165.644 147.907 151.303 155.874 138.788C159.743 132.414 163.156 125.361 164.976 116.481L166.34 110.791H174.535C192.745 110.791 208.449 96.0001 208.449 77.1048V73.6922C208.449 52.5266 195.25 41.3694 172.258 38.6421C159.287 37.5068 151.548 37.0501 114.672 37.0501C89.865 37.0501 72.1115 37.2784 58.6837 39.3272C39.7949 42.0609 32.2787 52.7547 32.2787 71.8719" fill="white"/>
      <path d="M166.348 87.5747C166.348 90.3084 168.397 92.3572 172.037 92.3572C183.645 92.3572 190.019 85.7544 190.019 74.8322C190.019 63.91 183.645 57.0787 172.037 57.0787C168.397 57.0787 166.348 59.1275 166.348 61.8612V87.5812V87.5747Z" fill="#202020"/>
      <path d="M54.5932 86.2052C54.5932 99.6327 62.1032 111.24 71.6617 120.348C78.036 126.495 88.0513 132.869 94.876 136.966C96.9248 138.102 98.9736 138.787 101.251 138.787C103.984 138.787 106.255 138.102 108.082 136.966C114.913 132.869 124.922 126.495 131.068 120.348C140.855 111.246 148.365 99.6392 148.365 86.2052C148.365 71.6358 137.443 58.6649 121.738 58.6649C112.408 58.6649 106.033 63.4473 101.251 70.0436C96.9248 63.4408 90.3285 58.6649 80.992 58.6649C65.0589 58.6649 54.587 71.6358 54.587 86.2052" fill="#FF5A16"/>
      </g>
      </g>
      <defs>
      <clipPath id="clip0_1_194">
      <rect width="737.184" height="200.806" fill="white" transform="translate(-0.00585938 0.299805)"/>
      </clipPath>
      </defs>
      </svg>
    ')

    globe_svg <- HTML('
    <svg viewBox="0 0 24 24" style="width: 20px; height: 20px; margin-right: 5px; vertical-align: middle;">
      <circle cx="12" cy="12" r="10" fill="#E0F7FA" stroke="#2C3E50" stroke-width="1.2"></circle>
      
      <path d="M12 2a14.5 14.5 0 0 0 0 20 14.5 14.5 0 0 0 0-20" fill="none" stroke="#2C3E50" stroke-width="1.2"></path>
      <path d="M2 12h20" fill="none" stroke="#2C3E50" stroke-width="1.2"></path>
    </svg>')

    chile_flag_svg <- HTML('
      <svg viewBox="0 0 30 20" width="22" height="14.7" style="border-radius: 2px; vertical-align: middle; margin-right: 5px;">
        <clipPath id="rounded-flag">
          <rect width="30" height="20" rx="2" ry="2"/>
        </clipPath>
        
        <g clip-path="url(#rounded-flag)">
          <rect width="30" height="10" fill="#FFFFFF"/>
          <rect width="30" height="10" y="10" fill="#D52B1E"/>
          <rect width="10" height="10" fill="#0039A6"/>
          <path d="M5 2.24 L5.63 4.18 L7.69 4.18 L6.02 5.38 L6.65 7.32 L5 6.12 L3.35 7.32 L3.98 5.38 L2.31 4.18 L4.37 4.18 Z" fill="#FFFFFF"/>
          
          <rect x="0.5" y="0.5" width="29" height="19" rx="2" ry="2" fill="none" stroke="#000000" stroke-width="1.0" opacity="0.2"/>
        </g>
      </svg>')
    # nolint end

    tags$footer(
      class = "custom-footer", # <--- Clase clave
      style = "text-align: center; padding: 30px 20px; font-size: 0.85em;
      line-height: 1.6;",

      hr(style = "width: 50%; margin: 0 auto 20px auto; opacity: 0.3;"),

      # Texto de la herramienta
      p(l$footer_text, style = "margin-bottom: 5px;"),

      # Link de metodología (ahora heredará color de la clase)
      actionLink("show_method", l$footer_link,
                 style = "font-weight: bold; text-decoration: underline;"),

      tags$br(),

      p(lang()$footer_support_msg,
        style = "margin: 25px auto 15px auto; font-weight: 500;
        max-width: 500px; line-height: 1.4; text-align: center;"),

      # Contenedor de Botones
      tags$div(
        style = "display: flex; justify-content: center; flex-wrap: wrap;
        gap: 20px; margin-bottom: 30px; align-items: flex-start;",

        # Bloque Mercado Pago
        tags$div(
          style = "display: flex; flex-direction: column; align-items: center; 
           gap: 4px; width: 200px;",
          tags$span(
            style = "font-size: 1.1em; text-align: center; line-height: 1.3;
            min-height: 50px; display: flex; flex-direction: column;
            justify-content: flex-end;",
            # Título con peso 600 y opacidad casi total
            tags$span(
              style = "font-weight: 600; opacity: 0.95; display: block;",
              chile_flag_svg,
              l$soporte_cl
            ),
            # Mensaje con peso 500 para diferenciar
            tags$span(
              style = "font-weight: 500; opacity: 0.8; font-size: 1.0em;",
              l$mp_msg
            )
          ),

          # Botón Mercado Pago
          tags$a(
            href = "https://link.mercadopago.cl/ezbiostatscl",
            target = "_blank",
            style = "text-decoration: none;",
            tags$div(style = "background-color: #FFE600; display: flex;
            align-items: center; justify-content: center; border-radius: 15px;
            width: 180px; height: 65px; box-shadow: 0 4px 10px rgba(0,0,0,0.1);
            overflow: hidden;",
              mp_svg
            )
          )
        ),

        # Bloque Ko-fi
        tags$div(
          style = "display: flex; flex-direction: column; align-items: center;
          gap: 4px; width: 200px;",

          tags$span(
            style = "font-size: 1.1em; text-align: center; line-height: 1.3;
                    min-height: 50px; display: flex; flex-direction: column;
                    justify-content: flex-end; align-items: center;",

            tags$span(
              style = "font-weight: 600; opacity: 0.95; display: flex;
              align-items: center; justify-content: center;",
              globe_svg,
              l$soporte_global
            ),

            tags$span(
              style = "font-weight: 500; opacity: 0.8; font-size: 1.0em;
              display: block; width: 100%;",
              l$kofi_msg
            )
          ),

          # Botón Ko-fi
          tags$a(
            href = "https://ko-fi.com/ezbiostats",
            target = "_blank",
            style = "text-decoration: none;",
            tags$div(style = "background-color: #FF6433; display: flex;
            align-items: center; justify-content: center; border-radius: 15px;
            width: 180px; height: 65px; box-shadow: 0 4px 10px rgba(0,0,0,0.1);
            overflow: hidden;",
              kofi_svg
            )
          )
        )
      ),

      # Aviso de seguridad (Clase terciaria para que sea sutil)
      p(l$footer_warning, class = "footer-warning",
        style = "font-size: 0.9em; font-style: italic;
        margin: 0 auto 25px auto; max-width: 400px"),

      # Copyright
      p(l$copyright, style = "opacity: 0.6; font-size: 0.9em;")
    )
  })

  # ===========================================================================
  # MÓDULO 3: RENDERIZADO DINÁMICO (ADAPTADO PARA FORMATO WIDE/LONG)
  # ===========================================================================

  # 3.1 Título Dinámico
  output$dinamic_title <- renderUI({
    req(lang())
    titlePanel(HTML(paste0(lang()$app_title, "<br><small style=\"color: #7f8c8d;
      font-size: 0.6em;\">", lang()$ver, "</small>")))
  })

  # 3.2 Input de Archivo (Incluye switch de formato y remoción de outliers)
  output$ui_archivo <- renderUI({
    req(lang())
    tagList(
      fileInput("archivo", lang()$step1, multiple = FALSE,
                accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv"),
                buttonLabel = lang()$browse, placeholder = lang()$placeholder),

      # Contenedor para el Switch de Formato Ancho/Largo
      div(style = "margin-bottom: -10px;",
        checkboxInput("datos_wide", lang()$wide_toggle, value = FALSE)
      ),
      helpText(style = "margin-top: 0px; margin-bottom: 15px;",
               tags$small(lang()$wide_help)),

      hr(),

      checkboxInput("remover_outliers", lang()$out_check, value = FALSE)
    )
  })

  # 3.3 Selectores de Variables (Soporte dinámico para Modo Wide)
  output$select_vars <- renderUI({
    req(df(), lang())

    if (input$datos_wide) {
      # Eliminamos el background hardcoded para que respete el Modo Oscuro
      return(wellPanel(
        style = "border-left: 4px solid #2C3E50;",
        textOutput("msg_wide_detect")
      ))
    }

    cols_numericas <- names(df())[sapply(df(), is.numeric)]
    cols_todas <- names(df())

    tagList(
      selectInput("var_x", lang()$v_x,
                  choices = cols_todas,
                  selected = cols_todas[1]),
      selectInput("var_y", lang()$v_y,
                  choices = cols_numericas,
                  selected = cols_numericas[1])
    )
  })

  output$msg_wide_detect <- renderText({
    lang()$msg_wide_auto
  })

  # 3.4 Personalización de Etiquetas (Pilar: Sincronización Inteligente)
  output$ui_custom_labels <- renderUI({
    req(df(), lang())

    # Determinamos los valores por defecto usando las nuevas claves del Módulo 1
    if (input$datos_wide) {
      default_x <- lang()$def_x_groups
      default_y <- lang()$def_y_values
    } else {
      # Si aún no hay selección, usamos el término genérico del diccionario
      default_x <- if (!is.null(input$var_x)) input$var_x
      else lang()$def_x_groups
      default_y <- if (!is.null(input$var_y)) input$var_y
      else lang()$def_y_values
    }

    wellPanel(
      strong(lang()$cust_title),
      textInput("custom_x", lang()$lab_x, value = default_x),
      textInput("custom_y", lang()$lab_y, value = default_y)
    )
  })

  # 3.5 y 3.6 Botonera (Sin cambios drásticos, solo limpieza de estilos)
  output$ui_boton_procesar <- renderUI({
    req(lang())
    actionButton("procesar", lang()$run, icon = icon("play"),
                 class = "btn-primary",
                 style = "width: 100%; margin-top: 10px;")
  })

  observeEvent(input$procesar, {
    shinyjs::disable("procesar")
    shinyjs::html("procesar", paste(icon("spinner", class = "fa-spin"),
                                    lang()$processing))
    delay(1000, {
      shinyjs::enable("procesar")
      shinyjs::html("procesar", paste(icon("play"), lang()$run))
    })
  })

  observeEvent(input$reset_all, {
    session$reload()
  })

  output$ui_boton_reset <- renderUI({
    req(lang())
    actionButton("reset_all", lang()$reset, icon = icon("undo"),
                 style = "width: 100%; margin-top: 20px; color: #fff;
                 background-color: #6c757d; border-color: #6c757d;")
  })

  # 3.7 Organización por Pestañas
  output$ui_tabs <- renderUI({
    req(df(), lang())
    tabsetPanel(
      tabPanel(lang()$tab_data, DTOutput("tabla_datos")),
      tabPanel(lang()$tab_plot, plotOutput("plot_final")),
      tabPanel(lang()$tab_rep, verbatimTextOutput("stats_res"))
    )
  })

  # ===========================================================================
  # MÓDULO 4: GESTIÓN DE DATOS Y PREPROCESAMIENTO (REVISADO Y COMPATIBLE)
  # ===========================================================================

  # 4.1 Objeto Reactivo de Datos (Importación y Pulido)
  df <- reactive({
    req(input$archivo)
    d <- import(input$archivo$datapath, encoding = "UTF-8") |> as_tibble()

    # A. Eliminar columnas que son TODO Na o puras celdas vacías
    d <- d[, colSums(is.na(d) | d == "") != nrow(d)]

    # B. Limpieza de nombres
    names(d) <- trimws(names(d))

    # C. Conversión robusta y limpieza de espacios en celdas
    d <- d |> mutate(across(everything(), ~ {
      if (is.numeric(.x)) return(.x)

      # 1. Limpieza básica
      x <- as.character(.x) |> trimws()
      x[x == ""] <- NA
      if (all(is.na(x))) return(x)

      # 2. DETECCIÓN UNIVERSAL DE TEXTO:
      # Intentamos convertir a número. Si la mayoría de los valores NO NA
      # no pueden convertirse a número, es una columna de categorías/texto.
      v_test <- suppressWarnings(as.numeric(gsub(",", ".", x)))

      # Si hay más valores de texto real que valores convertibles a número:
      if (sum(!is.na(x)) > sum(!is.na(v_test))) {
        return(x)
      }

      # 3. Tu lógica original de Punto vs Coma
      v_punto <- suppressWarnings(as.numeric(x))
      v_coma  <- suppressWarnings(as.numeric(gsub(",", ".", x)))

      if (sum(!is.na(v_punto)) >= sum(!is.na(v_coma))) {
        if (!all(is.na(v_punto))) return(v_punto)
      } else {
        if (!all(is.na(v_coma))) return(v_coma)
      }
      return(x)
    }))

    # D. Eliminar filas que quedaron vacías tras la limpieza
    d <- d |> filter(if_any(everything(), ~ !is.na(.)))

    return(d)
  })

  # 4.2 Auditoría de Outliers (Versión Blindada)
  df_auditado <- reactive({
    req(df())
    d_input <- df()

    if (input$datos_wide) {
      d_base <- d_input |>
        tidyr::pivot_longer(
          cols = where(is.numeric),
          names_to = "X_INTERNAL",
          values_to = "Y_INTERNAL"
        ) |>
        mutate(X_INTERNAL = factor(X_INTERNAL)) |>
        drop_na(X_INTERNAL, Y_INTERNAL)
    } else {
      # MODO LARGO: Aquí es donde fallaba.
      # Validamos que las columnas seleccionadas existan de verdad
      req(input$var_x, input$var_y)

      # SEGURO: Si la columna no existe, req() detiene todo silenciosamente
      req(input$var_x %in% names(d_input), input$var_y %in% names(d_input))

      d_base <- d_input |>
        mutate(X_INTERNAL = factor(!!sym(input$var_x)),
               Y_INTERNAL = as.numeric(!!sym(input$var_y))) |>
        drop_na(X_INTERNAL, Y_INTERNAL)
    }

    # Auditoría de outliers (Tukey)
    d <- d_base |>
      group_by(X_INTERNAL) |>
      mutate(q1 = quantile(Y_INTERNAL, 0.25, na.rm = TRUE),
             q3 = quantile(Y_INTERNAL, 0.75, na.rm = TRUE),
             iqr_val = q3 - q1,
             is_out = Y_INTERNAL < (q1 - 1.5 * iqr_val) |
               Y_INTERNAL > (q3 + 1.5 * iqr_val)) |>
      ungroup()

    return(d)
  })

  # 4.3 Renderizado de la Tabla (Versión Limpia y Centralizada)
  output$tabla_datos <- renderDT({
    req(df(), lang())
    d_original <- df()
    d_analisis <- tryCatch(df_auditado(), error = function(e) NULL)

    # 1. Preparación de Opciones usando lang()
    opciones_base <- list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = "ltip",
      initComplete = JS(
        "function(settings, json) {",
        "  var table = this.api();",
        "  setTimeout(function() { table.columns.adjust(); }, 500);",
        "}"
      ),
      # TRADUCCIÓN CENTRALIZADA AQUÍ:
      language = list(
        search = lang()$dt_search,
        lengthMenu = lang()$dt_length,
        info = lang()$dt_info,
        paginate = list(previous = lang()$dt_prev, `next` = lang()$dt_next)
      )
    )

    # 2. Selección de Data y Configuración de Columnas
    if (input$datos_wide || is.null(d_analisis)) {
      data_final <- d_original
    } else {
      data_final <- d_analisis
      # Ocultar columnas técnicas (Lógica exacta de la versión antigua)
      cols_to_hide <- which(names(data_final) %in%
                              c("X_INTERNAL", "Y_INTERNAL", "q1", "q3",
                                "iqr_val", "is_out")) - 1
      opciones_base$columnDefs <- list(list(visible = FALSE,
                                            targets = cols_to_hide))
    }

    # 3. Creación del objeto Datatable
    dt_obj <- datatable(data_final, rownames = FALSE, options = opciones_base)

    # 4. Formateo Condicional (El "brillo" de la versión nueva)
    if (!is.null(d_analisis)) {
      if (input$datos_wide) {
        # Modo Ancho: Colorear cada columna numérica si tiene outliers
        cols_num <- names(d_original)[sapply(d_original, is.numeric)]
        for (col in cols_num) {
          outlier_rows <- d_analisis |>
            filter(X_INTERNAL == col, is_out == TRUE) |>
            pull(Y_INTERNAL)

          if (length(outlier_rows) > 0) {
            dt_obj <- dt_obj |> formatStyle(col,
              backgroundColor = styleEqual(outlier_rows,
                                           rep("#FFF0F0",
                                               length(outlier_rows))),
              color = styleEqual(outlier_rows,
                                 rep("#D9534F", length(outlier_rows))),
              fontWeight = styleEqual(outlier_rows,
                                      rep("bold", length(outlier_rows)))
            )
          }
        }
      } else if (input$var_y %in% names(data_final)) {
        # Modo Largo: Colorear solo la Variable Y seleccionada
        dt_obj <- dt_obj |>
          formatStyle(input$var_y, "is_out",
                      backgroundColor = styleEqual(c(TRUE),
                                                   c("#FFF0F0")),
                      color = styleEqual(c(TRUE), c("#D9534F")),
                      fontWeight = styleEqual(c(TRUE), c("bold")))
      }
    }

    return(dt_obj)
  })

  # ===========================================================================
  # MÓDULO 5: MOTOR DE DECISIÓN ESTADÍSTICA (PARTE 1 - REINTEGRADA)
  # ===========================================================================

  analisis_obj <- eventReactive(list(input$procesar, input$archivo), {

    # 1. ¿Es un clic real?
    es_clic <- input$procesar > v_clic()
    v_clic(input$procesar) # Actualizamos el marcador

    # 2. Si se disparó por el archivo (o el botón no se movió), devolvemos NULL
    # Esto limpia el gráfico y el reporte al instante.
    if (!es_clic || is.null(input$archivo)) {
      return(NULL)
    }

    # 3. Solo si es un clic real, validamos que los datos estén listos
    # req(df_auditado()) aquí esperará a que termine de actualizarse
    req(df_auditado())

    # 5.1 Preparación y Reordenamiento Lógico Universal
    # PILAR: Usamos los nombres internos ya preparados en el Módulo 4
    d_base <- df_auditado() |>
      mutate(X_INTERNAL = droplevels(X_INTERNAL))

    orden_aparicion <- d_base |>
      pull(X_INTERNAL) |>
      as.character() |>
      unique()

    niveles <- levels(d_base$X_INTERNAL)

    get_num <- function(x) {
      m <- regmatches(x, regexpr("[-+]?[0-9]*\\.?[0-9]+", x))
      if (length(m) == 0) NA else as.numeric(m[1])
    }

    get_semantic_rank <- function(x) {
      x <- tolower(trimws(x))

      # 1. Definimos los patrones por nivel de importancia
      # Rank 1: El "Suelo" (Basales y Negativos)
      p_rank1 <- "^(con|ct|wt|veh|basal|ref|pool|blank|blanco|empty|sh|unt|saline|pbs|dmso|med|buff|input|mock|neg|c\\-)" # nolint: line_length_linter.

      # Rank 2: El "Techo" (Validaciones Positivas)
      p_rank2 <- "(pos|c\\+|std|est|max)"

      # Rank 5: Las "Alteraciones" (Mutantes, KO, Inhibidores)
      p_rank5 <- "(mut|antag|inhib|block|ko|sn|kd|null|inh|ant|rev|scr|\\bsi\\b|rna)" # nolint: line_length_linter.

      # 2. Lógica de discriminación en cascada (el orden de los IF importa)
      if (grepl(p_rank1, x)) {
        1
      } else if (grepl(p_rank2, x)) {
        2
      } else if (grepl(p_rank5, x)) {
        5
      } else {
        3 # Todo lo que sea un tratamiento normal queda al centro
      }
    }

    df_jerarquia <- data.frame(nivel = niveles) |>
      mutate(rank_sem = sapply(nivel, get_semantic_rank),
             valor_num = sapply(nivel, get_num),
             idx_custom = match(nivel, orden_aparicion)) |>
      arrange(rank_sem, valor_num, idx_custom, nivel)

    d_base <- d_base |>
      mutate(X_INTERNAL = factor(X_INTERNAL, levels = df_jerarquia$nivel))

    # 5.2 Bifurcación de Datos (Con lógica de enmascarados reintegrada)
    n_outliers <- sum(d_base$is_out)
    d_full <- d_base |> mutate(X_PLOT = X_INTERNAL, Y_PLOT = Y_INTERNAL)

    d_stats <- if (input$remover_outliers) d_full |> filter(!is_out) else d_full
    d_stats <- d_stats |> mutate(X_PLOT = droplevels(X_PLOT))

    # Lógica de outliers enmascarados que habías diseñado
    d_re_auditado <- d_stats |>
      group_by(X_PLOT) |>
      mutate(
        q1_new = quantile(Y_PLOT, 0.25, na.rm = TRUE),
        q3_new = quantile(Y_PLOT, 0.75, na.rm = TRUE),
        iqr_new = q3_new - q1_new,
        is_new_out = Y_PLOT < (q1_new - 1.5 * iqr_new) |
          Y_PLOT > (q3_new + 1.5 * iqr_new)
      ) |>
      ungroup()

    n_out_enmascarados <- sum(d_re_auditado$is_new_out)

    # Validaciones Críticas
    conteos_raw    <- as.numeric(table(d_stats$X_PLOT))
    conteos_reales <- conteos_raw[conteos_raw > 0]
    n_grupos_val   <- length(conteos_reales)
    n_min_val      <- if (n_grupos_val > 0) min(conteos_reales) else 0

    validate(
      need(n_grupos_val >= 2, lang()$err_factor),
      need(n_min_val >= 3, lang()$err_n_small)
    )

    # 5.3 Validación de Supuestos + Lógica TLC
    n_grupos <- n_grupos_val
    n_min    <- n_min_val

    # 1. Test de Shapiro por grupo
    norm_res <- d_stats |> group_by(X_PLOT) |> shapiro_test(Y_PLOT)

    # 2. Lógica TLC: Si n >= 30, la normalidad se asume por ley estadística
    # Esto sobreescribe el p-valor de Shapiro
    es_normal_por_shapiro <- all(norm_res$p > 0.05)
    es_normal_por_tlc     <- (n_min >= 30)

    # La normalidad es FIABLE si (n >= 5 y Shapiro OK) O si (n >= 30)
    es_fiable_normal <- (n_min >= 5 && es_normal_por_shapiro) ||
      es_normal_por_tlc

    # 3. Homocedasticidad (Levene)
    v_grupos <- d_stats |>
      group_by(X_PLOT) |>
      summarise(v = var(Y_PLOT, na.rm = TRUE)) |>
      pull(v)

    if (any(v_grupos == 0 | is.na(v_grupos))) {
      is_homo <- FALSE
      homo_res <- list(p = 0)
    } else {
      homo_res <- d_stats |> levene_test(Y_PLOT ~ X_PLOT)
      is_homo  <- homo_res$p > 0.05
    }

    # Barras solo si todo es "perfecto" (Normal + Homo + No outliers)
    usar_barras <- es_fiable_normal && is_homo && (n_outliers == 0)

    # 5.4 Inferencia Automática
    max_y <- max(d_full$Y_PLOT, na.rm = TRUE)

    # Variable para guardar la razón de la prueba (Para el reporte posterior)
    razon_prueba <- if (es_normal_por_tlc) {
      "TLC (n >= 30)"
    } else if (es_fiable_normal) {
      lang()$reason_ok
    } else if (n_min < 5) {
      lang()$reason_n
    } else {
      lang()$reason_norm
    }

    if (n_grupos == 2) {
      if (es_fiable_normal) {
        # Si es normal pero NO hay homocedasticidad, usa Welch automáticamente
        res <- d_stats |>
          t_test(Y_PLOT ~ X_PLOT, var.equal = is_homo) |>
          add_significance()
        method_used <- if (is_homo) lang()$test_t else lang()$test_w
      } else {
        res <- d_stats |> wilcox_test(Y_PLOT ~ X_PLOT) |> add_significance()
        method_used <- lang()$test_wilcox
      }
    } else {
      if (es_fiable_normal) {
        if (is_homo) {
          res <- d_stats |> anova_test(Y_PLOT ~ X_PLOT)
          method_used <- lang()$test_anova
          post_hoc <- d_stats |>
            tukey_hsd(Y_PLOT ~ X_PLOT) |>
            add_significance() |>
            add_y_position()
          ph_name <- lang()$ph_tukey
        } else {
          # SALVAVIDAS DE WELCH: ANOVA de Welch + Games-Howell
          res <- d_stats |> welch_anova_test(Y_PLOT ~ X_PLOT)
          method_used <- lang()$test_wanova
          post_hoc <- d_stats |>
            games_howell_test(Y_PLOT ~ X_PLOT) |>
            add_significance() |>
            add_y_position()
          ph_name <- lang()$ph_gh
        }
      } else {
        res <- d_stats |> kruskal_test(Y_PLOT ~ X_PLOT)
        method_used <- lang()$test_kw
        post_hoc <- d_stats |>
          dunn_test(Y_PLOT ~ X_PLOT) |>
          add_significance() |>
          add_y_position()
        ph_name <- lang()$ph_dunn
      }

      p_v <- post_hoc$p.adj
      names(p_v) <- paste0(post_hoc$group1, "-", post_hoc$group2)

      letras_obj <- multcompLetters(p_v)
      df_letras <- data.frame(
        X_PLOT = names(letras_obj$Letters),
        label = letras_obj$Letters,
        stringsAsFactors = FALSE
      )

      pos_letras <- d_stats |>
        group_by(X_PLOT) |>
        summarise(y_max_grupo = if (usar_barras) mean(Y_PLOT) + sd(Y_PLOT)
                  else max(Y_PLOT),
                  .groups = "drop")

      df_letras <- df_letras |>
        left_join(pos_letras, by = "X_PLOT") |>
        mutate(y_pos = y_max_grupo + (max_y * 0.05))

      ph_razon <- if (!es_fiable_normal) {
        if (n_min < 5) lang()$reason_n else lang()$reason_norm
      } else {
        lang()$reason_ok
      }
    }

    # 5.5 Construcción del Gráfico
    formas_prism <- c(21, 22, 23, 24, 25, 21, 22)
    p <- ggplot(d_stats, aes(x = X_PLOT, y = Y_PLOT, fill = X_PLOT))

    if (usar_barras) {
      p <- p + stat_summary(fun = mean, geom = "bar", width = 0.6,
                            color = "black", alpha = 0.8) +
        stat_summary(fun.data = mean_sd, geom = "errorbar",
                     width = 0.2)
    } else {
      valor_coef <- if (input$remover_outliers) 1.5 else 999
      p <- p + stat_boxplot(geom = "errorbar", width = 0.2,
                            coef = valor_coef) +
        geom_boxplot(width = 0.5, alpha = 0.5,
                     outlier.shape = NA, coef = valor_coef)
    }

    # PUNTOS: Usamos d_full para ver outliers aunque estén filtrados
    if (nrow(d_full) < 60) {
      p <- p + geom_beeswarm(data = d_full, aes(shape = X_PLOT),
                             cex = 1.2, priority = "density",
                             alpha = 0.8, size = 3.5,
                             stroke = 0.7)
    } else {
      p <- p + geom_jitter(data = d_full, aes(shape = X_PLOT), width = 0.2,
                           alpha = 0.6, size = 2.5,
                           stroke = 0.5)
    }

    # Significancia y Techo
    n_sig <- if (n_grupos > 2) nrow(post_hoc |> filter(p.adj < 0.05)) else 0
    usar_cld <- (n_sig > 3 || n_grupos > 4)

    if (n_grupos == 2) {
      p <- p + geom_bracket(
        data = data.frame(
          a = levels(d_stats$X_PLOT)[1],
          b = levels(d_stats$X_PLOT)[2],
          lab = res$p.signif,
          y = max_y * 1.1
        ),
        aes(xmin = a, xmax = b, label = lab, y.position = y),
        label.size = 5, tip.length = 0.03, color = "black",
        inherit.aes = FALSE
      )
      techo_final <- max_y * 1.25
    } else {
      if (usar_cld && exists("df_letras")) {
        p <- p + geom_text(data = df_letras,
                           aes(x = X_PLOT, y = y_pos, label = label),
                           vjust = 0, size = 5, fontface = "bold",
                           color = "black", inherit.aes = FALSE)
        techo_final <- max(df_letras$y_pos) * 1.15
      } else {
        post_hoc_sig <- post_hoc |> filter(p.adj < 0.05)
        if (nrow(post_hoc_sig) > 0) {
          paso <- max_y * 0.12
          post_hoc_sig$y_pos <- seq(max_y * 1.05, by = paso,
                                    length.out = nrow(post_hoc_sig))
          p <- p + geom_bracket(
            data = post_hoc_sig,
            aes(xmin = group1, xmax = group2, label = p.adj.signif,
                y.position = y_pos), label.size = 4.5, tip.length = 0.03,
            color = "black", inherit.aes = FALSE
          )
          techo_final <- max(post_hoc_sig$y_pos) * 1.15
        } else {
          techo_final <- max_y * 1.15
        }
      }
    }

    # Ejes y Escalas
    min_real <- min(d_full$Y_PLOT, na.rm = TRUE)
    if (usar_barras) {
      y_min <- 0
      expansiones <- expansion(mult = c(0, 0.05))
    } else {
      y_min <- min_real - (abs(min_real) * 0.1)
      if (y_min < 0 && min_real >= 0) y_min <- 0
      expansiones <- expansion(mult = c(0.05, 0.05))
    }

    raw_step <- (techo_final - y_min) / 5
    base_powers <- c(1, 2, 2.5, 5, 10)
    magnitude <- 10^floor(log10(raw_step))
    nice_steps <- base_powers * magnitude
    paso_final <- nice_steps[which.min(abs(nice_steps - raw_step))]
    marcas_y <- seq(floor(y_min / paso_final) * paso_final,
                    techo_final + paso_final, by = paso_final)
    marcas_y <- marcas_y[marcas_y <= techo_final * 1.05]

    p <- p + scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
      scale_shape_manual(values = formas_prism) + theme_prism() +
      theme(axis.text.x = if (n_grupos > 4) element_blank() else element_text(),
            axis.ticks.x = if (n_grupos > 4) element_blank()
            else element_line(),
            legend.position = if (n_grupos > 4) "right" else "none") +
      scale_y_continuous(limits = c(y_min, techo_final), breaks = marcas_y,
                         expand = expansiones, guide = "prism_offset_minor") +
      labs(x = input$custom_x, y = input$custom_y)

    # Retorno de objeto
    list(plot = p, stats = res, razon_prueba = razon_prueba,
         method = method_used, norm = norm_res, homo = homo_res,
         n_grupos = n_grupos, n_min = n_min, n_out = n_outliers,
         n_out_enmascarados = n_out_enmascarados,
         es_normal = es_fiable_normal, usar_barras = usar_barras,
         d_stats = d_stats, d_full = d_full,
         rem_out_status = input$remover_outliers,
         post_hoc = if (n_grupos > 2) post_hoc else NULL,
         ph_name = if (n_grupos > 2) ph_name else NULL,
         ph_razon = if (n_grupos > 2) ph_razon else NULL)
  })

  # ===========================================================================
  # MÓDULO 6: SALIDAS VISUALES Y DESCARGAS (COMPATIBLE CON MODO OSCURO)
  # ===========================================================================

  # 6.1 Renderizado en UI Ultra-Veloz y Dinámico
  output$plot_final <- renderPlot({
    res <- analisis_obj()
    req(res)

    # 1. Recuperamos el gráfico base (que viene en negro del 5.5)
    p <- res$plot

    # 2. Detectamos el color actual del switch (esto es reactivo e instantáneo)
    # Si el usuario cambia el switch, este bloque se ejecuta solo.
    col_txt <- if (input$dark_mode == "light") "black" else "white"

    # 3. Sobreescribimos el tema de forma masiva
    p_preview <- p +
      theme(
        # Forzamos color en todo el texto y líneas de ejes (vence a ggprism)
        text = element_text(color = col_txt),
        axis.text = element_text(color = col_txt),
        axis.title = element_text(color = col_txt),
        axis.line = element_line(color = col_txt),
        axis.ticks = element_line(color = col_txt),
        # Fondos transparentes
        panel.background = element_blank(),
        plot.background  = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()
      )

    # 4. El "Pincel" Mágico: Cambiamos el color de los Geoms sin reconstruirlos
    # Esto cambia barras, corchetes, letras y puntos de forma ultra rápida
    p_preview$layers <- lapply(p_preview$layers, function(l) {
      # Si la capa tiene un color definido (como el negro que pusimos en 5.5)
      # lo cambiamos al color actual del switch.
      if (!is.null(l$aes_params$colour)) {
        l$aes_params$colour <- col_txt
      }
      l
    })

    suppressWarnings(print(p_preview))
  }, bg = "transparent", res = 96)

  # 6.2 UI de Botonera de Descarga (Refactorizada)
  output$download_ui <- renderUI({
    res <- analisis_obj()
    req(res, lang())

    fluidRow(
      column(6,
        downloadButton("download_tiff", lang()$btn_down_fig, # Usamos clave
                       class = "btn-success",
                       style = "width: 100%; margin-top: 10px;")
      ),
      column(6,
        downloadButton("download_report", lang()$btn_down_rep, # Usamos clave
                       class = "btn-info",
                       style = "width: 100%; margin-top: 10px;")
      )
    )
  })

  # 6.3 Manejador de la descarga TIFF (Ajustado para Boxplots y Símbolos)
  output$download_tiff <- downloadHandler(
    filename = function() {
      paste0("Fig_", format(Sys.Date(), "%d-%m-%Y"), ".tiff")
    },
    content = function(file) {
      res_down <- analisis_obj()
      req(res_down) # Seguridad extra

      p_original <- res_down$plot

      # RESET AGRESIVO DE COLORES
      # Recorremos cada capa para capturar bordes de cajas, bigotes y símbolos
      p_original$layers <- lapply(p_original$layers, function(l) {
        # 1. Cambiar en aes_params (lo más común)
        if (!is.null(l$aes_params$colour)) {
          l$aes_params$colour <- "black"
        }
        # 2. Cambiar en geom_params
        if (!is.null(l$geom_params$colour)) {
          l$geom_params$colour <- "black"
        }
        l
      })

      # Limpieza de fondos y forzado de texto
      p_export <- p_original +
        theme(
          text = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank()
        )

      tryCatch({
        dev_type <- if (requireNamespace("ragg", quietly = TRUE)) "ragg_tiff"
        else "tiff"

        ggsave(
          file,
          plot = p_export,
          device = dev_type,
          width = 7,
          height = 5,
          units = "in",
          dpi = 300,
          compression = "lzw",
          bg = "transparent"
        )
      }, error = function(e) {
        ggsave(file, plot = p_export, device = "png",
               width = 7, height = 5, dpi = 300, bg = "transparent")
      })
    }
  )

  # ===========================================================================
  # MÓDULO 7: GENERADOR DE REPORTES (REVISADO Y ROBUSTO)
  # ===========================================================================

  # 7.1 Función Maestra de Generación de Texto
  generar_cuerpo_reporte <- function(analisis_obj, lang, input) {
    m <- analisis_obj
    l <- lang
    d <- m$d_stats

    if (is.null(d) || nrow(d) == 0) return(NULL)

    ancho <- 80
    sep <- paste0(rep("=", ancho), collapse = "")
    sub_sep <- paste0(rep("-", ancho), collapse = "")

    centrar <- function(texto, width) {
      texto_limpio <- as.character(texto)
      n <- (width - nchar(texto_limpio)) / 2
      if (n < 0) return(texto_limpio)
      paste0(paste0(rep(" ", floor(n)), collapse = ""), texto_limpio)
    }

    alinear <- function(label, valor) {
      sprintf("%-18s : %s", label, valor)
    }

    # 7.1.1 ENCABEZADO Y ESTADÍSTICOS DESCRIPTIVOS
    cat(sep, "\n", sep = "")
    cat(centrar(l$tit_rep, ancho), "\n", sep = "")
    cat(sep, "\n\n", sep = "")

    y_label <- if (!is.null(input$custom_y) && nzchar(input$custom_y))
      input$custom_y else l$def_resp
    cat(alinear(l$var_ana, y_label), "\n", sep = "")

    # Lógica de descriptivos (Media/Mediana)
    if (m$usar_barras) {
      stats_det <- d |>
        group_by(X_PLOT) |>
        summarise(m1 = mean(Y_PLOT), m2 = sd(Y_PLOT), n_val = n(),
                  .groups = "drop")
    } else {
      stats_det <- d |>
        group_by(X_PLOT) |>
        summarise(m1 = median(Y_PLOT), q1 = quantile(Y_PLOT, 0.25),
                  q3 = quantile(Y_PLOT, 0.75),
                  mean_val = mean(Y_PLOT), sd_val = sd(Y_PLOT), n_val = n(),
                  .groups = "drop") |>
        mutate(m2_text = paste0(round(q1, 2), " - ", round(q3, 2)))
    }

    for (i in seq_len(nrow(stats_det))) {
      nombre_grupo <- substr(as.character(stats_det$X_PLOT[i]), 1, 18)
      if (m$usar_barras) {
        val_display <- paste0(round(stats_det$m1[i], 2), " ± ",
                              round(stats_det$m2[i], 2))
        cat(sprintf("%2d. %-18s : %s %s (n=%d)\n", i, nombre_grupo, val_display,
                    l$msd, stats_det$n_val[i]))
      } else {
        val_mediana <- paste0(round(stats_det$m1[i], 2),
                              " [", stats_det$m2_text[i], "]")
        val_media   <- paste0(round(stats_det$mean_val[i], 2), " ± ",
                              round(stats_det$sd_val[i], 2))
        cat(sprintf("%2d. %-18s : %s %s (n=%d)\n", i, nombre_grupo,
                    val_mediana, l$med_iqr, stats_det$n_val[i]))
        cat(sprintf("    %-19s : %s\n", l$msd, val_media))
      }
    }

    # 7.1.2 OBSERVACIONES METODOLÓGICAS (ALERTAS)
    cat(sub_sep, "\n", sep = "")
    cat(centrar(l$tit_obs, ancho), "\n\n", sep = "")

    alertas <- c()

    # A. Tamaño de Muestra
    if (m$n_min < 5) {
      alertas <- c(alertas, l$warn_n_crit)
    } else if (m$n_min < 10) {
      alertas <- c(alertas, l$warn_n_low)
    }

    # B. Desequilibrio de n
    n_max <- if (nrow(stats_det) > 0) max(stats_det$n_val) else 0
    if (!is.null(m$n_min) && m$n_min > 0 && (n_max / m$n_min > 1.5)) {
      alertas <- c(alertas, l$warn_unbalanced)
    }

    # C. Decisiones Técnicas (TLC / Welch)
    if (!is.null(m$razon_prueba) && m$razon_prueba == "TLC (n >= 30)") {
      alertas <- c(alertas, l$info_tlc)
    }
    if (m$method == l$test_w || m$method == l$test_wanova) {
      alertas <- c(alertas, l$info_welch)
    }

    # Impresión de Alertas formateadas
    if (length(alertas) > 0) {
      for (a in alertas) {
        texto_envuelto <- strwrap(a, width = ancho - 6)
        cat(" [!] ", texto_envuelto[1], "\n", sep = "")
        if (length(texto_envuelto) > 1) {
          for (j in 2:length(texto_envuelto))
            cat("     ", texto_envuelto[j], "\n", sep = "")
        }
      }
    } else {
      cat(l$msg_robust, "\n")
    }

    # 7.1.3 SECCIÓN DE OUTLIERS
    cat(sub_sep, "\n", sep = "")
    cat(centrar(l$out_rep, ancho), "\n\n", sep = "")
    cat(paste0(l$out_method, "\n"))
    cat(paste0(l$out_count, " : ", m$n_out, "\n\n"))

    if (!is.null(m$n_out) && m$n_out > 0) {
      # Usamos m$rem_out_status para la decisión
      cat(strwrap(if (m$rem_out_status) l$out_rem
                  else l$out_keep, width = ancho), sep = "\n")
      if (m$rem_out_status && !is.null(m$n_out_enmascarados) &&
            m$n_out_enmascarados > 0) {
        cat(l$out_mask_tit, "\n")
        cat(strwrap(sprintf(l$out_mask_msg, m$n_out_enmascarados),
                    width = ancho), sep = "\n")
      }
    } else {
      cat(strwrap(l$out_clean, width = ancho), sep = "\n")
    }

    # 7.1.4 VALIDACIÓN DE SUPUESTOS
    cat(sub_sep, "\n", sep = "")
    cat(centrar(l$tit_sup, ancho), "\n\n", sep = "")

    cat(l$norm, "\n", sep = "")
    for (i in seq_len(nrow(m$norm))) {
      cat(sprintf("%-18s %s %s\n",
                  paste0("[", substr(as.character(m$norm$X_PLOT[i]), 1, 14),
                         "]"),
                  l$p_lab, round(m$norm$p[i], 4)))
    }

    cat("\n", l$homo, "\n", sep = "")
    cat(sprintf("%s %s\n", l$homo_lab, round(m$homo$p, 4)))
    cat("\n", l$nota_label, " ", l$crit_dec, "\n\n", sep = "")

    cumple_homo <- if (!is.null(m$homo$p)) m$homo$p > 0.05 else FALSE
    cat(strwrap(if (m$es_normal && cumple_homo) paste(l$sup_ok)
                else paste(l$sup_fail),
                width = ancho), sep = "\n")

    # 7.1.5 INFERENCIA
    cat(sub_sep, "\n", sep = "")
    cat(centrar(l$tit_test, ancho), "\n\n", sep = "")

    if (m$n_grupos == 2) {
      cat(alinear(l$grupos, paste(m$stats$group1, "vs", m$stats$group2)),
          "\n", sep = "")
    } else {
      cat(alinear(l$grupos, paste(m$n_grupos, l$niveles_txt)), "\n", sep = "")
    }

    cat(alinear(l$prueba, m$method), "\n", sep = "")
    p_tag <- if (m$n_grupos > 2) l$p_global else l$pval
    cat(alinear(p_tag, format.pval(m$stats$p, digits = 3, eps = 0.001,
                                   scientific = FALSE)),
        "\n", sep = "")

    sig_stars <- if (m$stats$p < 0.0001) "****"
    else if (m$stats$p < 0.001) "***"
    else if (m$stats$p < 0.01) "**"
    else if (m$stats$p < 0.05) "*"
    else "ns"
    cat(alinear(l$sig, sig_stars), "\n\n", sep = "")

    cat(l$interp, ":\n", sep = "")
    cat(strwrap(if (m$stats$p < 0.05) l$pos else l$neg, width = ancho),
        sep = "\n")

    # 7.1.6 POST-HOC
    if (m$n_grupos > 2 && m$stats$p < 0.05 && !is.null(m$post_hoc)) {
      cat(sub_sep, "\n", sep = "")
      cat(centrar(l$pair_comp, ancho), "\n\n", sep = "")
      cat(l$ph_test_lab, " ", m$ph_name, "\n", sep = "")
      cat(strwrap(paste0(l$just, " : ", m$ph_razon), width = ancho),
          sep = "\n\n")

      comp_txt <- paste0(m$post_hoc$group1, " vs ", m$post_hoc$group2)
      ancho_comp <- max(nchar(comp_txt)) + 1
      for (i in seq_len(nrow(m$post_hoc))) {
        cat(sprintf(paste0("%-", ancho_comp, "s: p = %-8s %s\n"),
                    comp_txt[i],
                    format.pval(m$post_hoc$p.adj[i], digits = 3, eps = 0.001,
                                scientific = FALSE),
                    m$post_hoc$p.adj.signif[i]))
      }
      cat(sub_sep, "\n", sep = "")
    }
  }

  # 7.2 Renderizado en la Interfaz
  output$stats_res <- renderPrint({
    # 1. Recuperamos el objeto (que se vuelve NULL si cambias el archivo)
    res <- analisis_obj()
    # 2. Si res es NULL, req() detiene el proceso y se limpia el área de texto
    req(res)
    # 3. Llamamos a tu función con el objeto validado
    generar_cuerpo_reporte(res, lang(), input)
  })

  # 7.3 Manejador de Descarga del Reporte
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Reporte_", format(Sys.Date(), "%d-%m-%Y"), ".txt")
    },
    content = function(file) {
      # Llamamos al objeto reactivo para que los datos estén disponibles
      res_reporte <- isolate(analisis_obj())
      req(res_reporte)

      lineas <- capture.output(
        generar_cuerpo_reporte(res_reporte, lang(), input)
      )
      writeLines(lineas, file)
    }
  )

  # 7.4 Modal de Metodología y Referencias
  # nolint start
  observeEvent(input$show_method, {
    showModal(modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,
      tags$div(
        style = "padding: 10px;",
        HTML(if (idioma_rv() == "ES") {
          "<h5><b>Software y Librerías Core</b></h5>
          <hr>
          <p>Desarrollado en <b>R</b> (v4.5+) con entorno <i>Shiny</i>. Paquetes: <i>rstatix, ggplot2, ggprism, ggpubr, bslib</i>.</p>

          <h5><b>Metodología Estadística y Flujo de Decisión</b></h5>
          <hr>
          <p>EZ Biostats implementa un flujo automatizado basado en estándares de la comunidad científica:</p>
          <ul>
            <li><b>Normalidad:</b> Evaluada mediante la prueba de <i>Shapiro-Wilk</i>. En muestras grandes (n ≥ 30), se asume normalidad por el Teorema del Límite Central.</li>
            <li><b>Homocedasticidad:</b> Prueba de <i>Levene</i>. Se aplica corrección de <i>Welch</i> por defecto ante varianzas desiguales.</li>
            <li><b>Outliers:</b> Identificados mediante el criterio de <i>Tukey</i><br>(Rango Intercuartílico x 1.5).</li>
          </ul>

          <h5><b>Referencias Bibliográficas (APA 7ma Ed.)</b></h5>
          <hr>
          <div style='font-size: 0.82em; color: #444; line-height: 1.4;'>
            <p><b>Criterios Estadísticos:</b><br>
            - Delacre, M., Lakens, D., & Leys, C. (2017). Why psychologists should by default use Welch’s t-test instead of Student’s t-test. <i>International Review of Social Psychology</i>, 30(1), 92-101. <a href='https://doi.org/10.5334/irsp.82' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.5334/irsp.82</a><br>
            - Diez, D. M., Barr, C. D., & Çetinkaya-Rundel, M. (2019). <i>OpenIntro Statistics</i> (4th ed.). OpenIntro. <a href='https://www.openintro.org/book/os/' target='_blank' style='text-decoration: none; font-weight: bold;'>https://www.openintro.org/book/os/</a><br>
            - Ghasemi, A., & Zahediasl, S. (2012). Normality tests for statistical analysis: A guide for non-statisticians. <i>International Journal of Endocrinology and Metabolism</i>, 10(2), 486-489. <a href='https://doi.org/10.5812/ijem.3505' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.5812/ijem.3505</a><br>
            - Kwak, S. G., & Kim, J. H. (2017). Central limit theorem: The cornerstone of modern statistics. <i>Korean Journal of Anesthesiology</i>, 70(2), 144-156. <a href='https://doi.org/10.4097/kjae.2017.70.2.144' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.4097/kjae.2017.70.2.144</a><br>
            - Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). <i>Biometrika</i>, 52(3/4), 591-611. <a href='https://doi.org/10.2307/2333709' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.2307/2333709</a><br>
            - Zar, J. H. (2010). <i>Biostatistical Analysis</i> (5th ed.). Pearson Prentice Hall.</p>

            <p><b>Visualización y Software:</b><br>
            - Kassambara, A. (2025). <i>rstatix: Pipe-friendly framework for basic statistical tests</i> (R package v0.7.3). <a href='https://CRAN.R-project.org/package=rstatix' target='_blank' style='text-decoration: none; font-weight: bold;'>https://CRAN.R-project.org/package=rstatix</a><br>
            - Weissgerber, T. L., Milic, N. M., Winham, S. J., & Garovic, V. D. (2015). Beyond bar and line graphs: Time for a new data presentation paradigm. <i>PLOS Biology</i>, 13(4), e1002128. <a href='https://doi.org/10.1371/journal.pbio.1002128' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.1371/journal.pbio.1002128</a><br>
            - Wickham, H., et al. (2019). Welcome to the Tidyverse. <i>Journal of Open Source Software</i>, 4(43), 1686. <a href='https://doi.org/10.21105/joss.01686' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.21105/joss.01686</a></p>
          </div>"
        } else {
          "<h5><b>Software & Core Packages</b></h5>
          <hr>
          <p>Developed in <b>R</b> (v4.5+) using the <i>Shiny</i> framework. Main packages: <i>rstatix, ggplot2, ggprism, ggpubr, bslib</i>.</p>

          <h5><b>Statistical Methodology & Decision Workflow</b></h5>
          <hr>
          <p>EZ Biostats implements an automated workflow based on scientific community standards:</p>
          <ul>
            <li><b>Normality:</b> Evaluated via <i>Shapiro-Wilk</i> test. For large samples (n ≥ 30), normality is assumed by the Central Limit Theorem.</li>
            <li><b>Homoscedasticity:</b> <i>Levene’s test</i>. <i>Welch’s</i> correction is applied by default in case of unequal variances.</li>
            <li><b>Outliers:</b> Identified using <i>Tukey’s</i> criterion (Interquartile Range x 1.5).</li>
          </ul>

          <h5><b>Bibliographic References (APA 7th Ed.)</b></h5>
          <hr>
          <div style='font-size: 0.82em; color: #444; line-height: 1.4;'>
            <p><b>Statistical Criteria:</b><br>
            - Delacre, M., Lakens, D., & Leys, C. (2017). Why psychologists should by default use Welch’s t-test instead of Student’s t-test. <i>International Review of Social Psychology</i>, 30(1), 92-101. <a href='https://doi.org/10.5334/irsp.82' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.5334/irsp.82</a><br>
            - Diez, D. M., Barr, C. D., & Çetinkaya-Rundel, M. (2019). <i>OpenIntro Statistics</i> (4th ed.). OpenIntro. <a href='https://www.openintro.org/book/os/' target='_blank' style='text-decoration: none; font-weight: bold;'>https://www.openintro.org/book/os/</a><br>
            - Ghasemi, A., & Zahediasl, S. (2012). Normality tests for statistical analysis: A guide for non-statisticians. <i>International Journal of Endocrinology and Metabolism</i>, 10(2), 486-489. <a href='https://doi.org/10.5812/ijem.3505' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.5812/ijem.3505</a><br>
            - Kwak, S. G., & Kim, J. H. (2017). Central limit theorem: The cornerstone of modern statistics. <i>Korean Journal of Anesthesiology</i>, 70(2), 144-156. <a href='https://doi.org/10.4097/kjae.2017.70.2.144' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.4097/kjae.2017.70.2.144</a><br>
            - Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). <i>Biometrika</i>, 52(3/4), 591-611. <a href='https://doi.org/10.2307/2333709' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.2307/2333709</a><br>
            - Zar, J. H. (2010). <i>Biostatistical Analysis</i> (5th ed.). Pearson Prentice Hall.</p>

            <p><b>Visualization & Software:</b><br>
            - Kassambara, A. (2025). <i>rstatix: Pipe-friendly framework for basic statistical tests</i> (R package v0.7.3). <a href='https://CRAN.R-project.org/package=rstatix' target='_blank' style='text-decoration: none; font-weight: bold;'>https://CRAN.R-project.org/package=rstatix</a><br>
            - Weissgerber, T. L., Milic, N. M., Winham, S. J., & Garovic, V. D. (2015). Beyond bar and line graphs: Time for a new data presentation paradigm. <i>PLOS Biology</i>, 13(4), e1002128. <a href='https://doi.org/10.1371/journal.pbio.1002128' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.1371/journal.pbio.1002128</a><br>
            - Wickham, H., et al. (2019). Welcome to the Tidyverse. <i>Journal of Open Source Software</i>, 4(43), 1686. <a href='https://doi.org/10.21105/joss.01686' target='_blank' style='text-decoration: none; font-weight: bold;'>https://doi.org/10.21105/joss.01686</a></p>
          </div>"
        })
      ),
      footer = modalButton(if (idioma_rv() == "ES") "Cerrar" else "Close")
    ))
  })
  # nolint end
}

# =============================================================================
# LANZAMIENTO DE LA APLICACIÓN
# =============================================================================
shinyApp(ui = ui, server = server)