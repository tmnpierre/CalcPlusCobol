      *    *************************************************************
      *    PROGRAMME CALCPLUS
      *    Ce programme permet à l'utilisateur de réaliser des 
      *    opérations arithmétiques basiques ainsi que des calculs de 
      *    puissance sur des nombres décimaux ou entiers. L'utilisateur 
      *    peut entrer une unité de valeurs, choisir les opérations, et
      *    voir le calcul en temps réel. Le programme supporte la 
      *    continuité des calculs et guide l'utilisateur en cas de      
      *    mauvaise saisie.
      *    *************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CalcPlus.
       AUTHOR. Pierre.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *    Définition des variables utilisées pour les calculs.
       01 WS-MAIN-VARIABLES.

      *    Le nombre actuellement saisi par l'utilisateur.
           05 WS-CURRENT-NUM       PIC 9(3)V99.

      *    Stocke le résultat du calcul précédent ou initial.          
           05 WS-PREVIOUS-RESULT   PIC 9(5)V99 VALUE ZERO.
           
      *    Résultat temporaire utilisé pour stocker le résultat des 
      *    calculs en cours.     
           05 WS-TEMP-RESULT       PIC 9(5)V99.

      *    Variables pour l'affichage formaté des nombres.
           05 WS-DISPLAY-PREV      PIC -ZZZ9.99.
           05 WS-DISPLAY-CURR      PIC -ZZZ9.99.

      *    Saisie utilisateur et commandes de contrôle.
       01 WS-USER-INPUTS-AND-CONTROLS.

           05 WS-USER-INPUT        PIC X(10).

      *    Entrée de l'utilisateur pour les nombres et les commandes.
           05 WS-OPERATION         PIC X.
      
      *    Type d'opération arithmétique sélectionné par l'utilisateur.
              88 ADDITION         VALUE 'A'.
              88 SUBTRACTION      VALUE 'S'.
              88 MULTIPLICATION   VALUE 'M'.
              88 DIV              VALUE 'D'.
              88 POWER            VALUE 'P'.

      *    Conditions spéciales pour chaque type d'opération.
           05 WS-CONTINUE          PIC X VALUE 'Y'.
              
      *    Indicateur pour continuer ou terminer les calculs.
              88 CONTINUE-CALC    VALUE 'Y', 'N'.

      *    Conditions pour contrôler la continuation ou la sortie du 
      *    programme.       
              88 EXIT-PROGRAM     VALUE 'E'.

      *    Formats d'affichage pour les nombres.
       01 WS-DISPLAY-FORMATS.

      *    Format d'affichage pour les résultats, supprime les zéros 
      *    non significatifs.     
           05 WS-NUM-DISPLAY       PIC -ZZZ9.99.
      
      *    Peut être utilisé pour un affichage alternatif ou 
      *    supplémentaire.
           05 WS-DISPLAY-NUM       PIC -ZZZ9.99.
              
       PROCEDURE DIVISION.

      *    Point d'entrée principal du programme.
       1000-MAIN.
           
      *    Initialisation du programme et affichage de bienvenue.
           PERFORM 1100-INITIALIZE.

      *    Demande à l'utilisateur d'entrer la première valeur.
           PERFORM 1200-FIRST-INPUT.

      *    Boucle principale pour le traitement des calculs
      *    jusqu'à ce que l'utilisateur décide de quitter le programme.
           PERFORM 1300-PROCESS-CALCULATIONS UNTIL EXIT-PROGRAM.

      *    Termine proprement le programme.
           PERFORM 9900-TERMINATE.

       1100-INITIALIZE.
      *    Affiche un message de bienvenue pour l'utilisateur.
           DISPLAY "Bienvenue dans CALCPLUS." SPACE WITH NO ADVANCING.

      *    Initialise la variable de contrôle pour continuer les calculs.
           MOVE 'Y' TO WS-CONTINUE.

       1200-FIRST-INPUT.
      *    Invite l'utilisateur à saisir la première valeur numérique.
           DISPLAY "Entrez la première valeur: " WITH NO ADVANCING.

      *    Accepte l'entrée de l'utilisateur et la stocke dans 
      *    WS-USER-INPUT.
           ACCEPT WS-USER-INPUT.

      *    Convertit l'entrée utilisateur en valeur numérique et 
      *    la stocke.
           MOVE FUNCTION NUMVAL(WS-USER-INPUT) TO WS-PREVIOUS-RESULT.
           MOVE WS-PREVIOUS-RESULT TO WS-DISPLAY-PREV.

       1300-PROCESS-CALCULATIONS.
      *    Affiche le résultat actuel pour l'utilisateur.
           MOVE WS-PREVIOUS-RESULT TO WS-NUM-DISPLAY.
           DISPLAY "Le résultat actuel est: " 
                   FUNCTION TRIM(WS-NUM-DISPLAY).

      *    Demande à l'utilisateur d'entrer une nouvelle valeur.
           DISPLAY "Entrez la prochaine valeur ou 'E' pour sortir: " 
                    WITH NO ADVANCING.
           ACCEPT WS-USER-INPUT.

      *    Vérifie si l'utilisateur souhaite sortir du programme.
           IF FUNCTION UPPER-CASE(WS-USER-INPUT) = "E"
              GO TO 9900-TERMINATE
           END-IF.

           MOVE FUNCTION NUMVAL(WS-USER-INPUT) TO WS-CURRENT-NUM.
           MOVE WS-CURRENT-NUM TO WS-DISPLAY-CURR.

      *    Demande à l'utilisateur de choisir une opération ou de sortir
           DISPLAY "Choisir l'opération [A/S/M/D/P] ou 'E' pour "
                    "sortir: " WITH NO ADVANCING.
           ACCEPT WS-OPERATION.

      *    Convertit l'opération saisie en majuscules.
           MOVE FUNCTION UPPER-CASE(WS-OPERATION) TO WS-OPERATION.

      *    Exécute l'opération choisie par l'utilisateur.
           PERFORM 1400-EXECUTE-OPERATION.

      *    Vérifie si l'utilisateur souhaite continuer les calculs.
           PERFORM 1500-CHECK-CONTINUE.



      *    Exécution de l'opération choisie par l'utilisateur.
       1400-EXECUTE-OPERATION.
      
      *    Exécute une opération arithmétique en fonction de l'opération
      *    saisie.
           EVALUATE WS-OPERATION
              WHEN 'A'
                 PERFORM 2100-DO-ADDITION
              WHEN 'S'
                 PERFORM 2200-DO-SUBTRACTION
              WHEN 'M'
                 PERFORM 2300-DO-MULTIPLICATION
              WHEN 'D'
                 PERFORM 2400-DO-DIV
              WHEN 'P'
                 PERFORM 2500-DO-POWER
              WHEN 'E'
                 PERFORM 9900-TERMINATE
              WHEN OTHER
                 DISPLAY "Opération invalide. Veuillez réessayer." 
                 SPACE WITH NO ADVANCING
           END-EVALUATE.

      *    Vérification si l'utilisateur souhaite continuer.
       1500-CHECK-CONTINUE.
      
      *    Demande à l'utilisateur s'il souhaite continuer.
           DISPLAY "Voulez-vous continuer avec ce résultat? (Y/N/E):" 
                    SPACE WITH NO ADVANCING.
           ACCEPT WS-CONTINUE.
           MOVE FUNCTION UPPER-CASE(WS-CONTINUE) TO WS-CONTINUE.
        
      *    Évalue la réponse de l'utilisateur et agit en conséquence.
           EVALUATE WS-CONTINUE
              WHEN 'Y'
                 PERFORM 1300-PROCESS-CALCULATIONS
              WHEN 'N'
                 PERFORM 1200-FIRST-INPUT
              WHEN 'E'
                 PERFORM 9900-TERMINATE
              WHEN OTHER
                 DISPLAY "Saisie invalide. Veuillez répondre par Y,"
                          SPACE "N ou E. " SPACE WITH NO ADVANCING
                 PERFORM 1500-CHECK-CONTINUE
           END-EVALUATE.

      *    Addition.
       2100-DO-ADDITION.
      
      *    Effectue une addition et affiche le résultat.
           DISPLAY "ADDITION DEMANDÉE: " WS-DISPLAY-PREV  " + " 
                   WS-DISPLAY-CURR
           COMPUTE WS-TEMP-RESULT = WS-PREVIOUS-RESULT + WS-CURRENT-NUM.
           MOVE WS-TEMP-RESULT TO WS-NUM-DISPLAY.
           DISPLAY "Addition: " FUNCTION TRIM(WS-NUM-DISPLAY).
           MOVE WS-TEMP-RESULT TO WS-PREVIOUS-RESULT.

      *    Soustraction.
       2200-DO-SUBTRACTION.
      
      *    Effectue une soustraction et affiche le résultat.
           DISPLAY "SOUSTRACTION DEMANDÉE: " WS-DISPLAY-PREV  " - " 
                   WS-DISPLAY-CURR
           COMPUTE WS-TEMP-RESULT = WS-PREVIOUS-RESULT - WS-CURRENT-NUM.
           MOVE WS-TEMP-RESULT TO WS-NUM-DISPLAY.
           DISPLAY "Soustraction: " FUNCTION TRIM(WS-NUM-DISPLAY).
           MOVE WS-TEMP-RESULT TO WS-PREVIOUS-RESULT.

      *    Multiplication.
       2300-DO-MULTIPLICATION.
      
      *    Effectue une multiplication et affiche le résultat.
           DISPLAY "MULTIPLICATION DEMANDÉE: " WS-DISPLAY-PREV " x " 
                   WS-DISPLAY-CURR
           COMPUTE WS-TEMP-RESULT = WS-PREVIOUS-RESULT * WS-CURRENT-NUM.
           MOVE WS-TEMP-RESULT TO WS-NUM-DISPLAY.
           DISPLAY "Multiplication: " FUNCTION TRIM(WS-NUM-DISPLAY).
           MOVE WS-TEMP-RESULT TO WS-PREVIOUS-RESULT.

      *    Division.
       2400-DO-DIV.
      
      *    Vérifie si la division par zéro est évitée et affiche le 
      *    résultat.
           IF WS-CURRENT-NUM = 0 THEN
              DISPLAY "Erreur: Division par zéro."
           ELSE
               DISPLAY "DIVISION DEMANDÉE: " WS-DISPLAY-PREV  " / " 
                   WS-DISPLAY-CURR
              COMPUTE WS-TEMP-RESULT = WS-PREVIOUS-RESULT / 
      -               WS-CURRENT-NUM
              MOVE WS-TEMP-RESULT TO WS-NUM-DISPLAY
              DISPLAY "Division: " FUNCTION TRIM(WS-NUM-DISPLAY)
              MOVE WS-TEMP-RESULT TO WS-PREVIOUS-RESULT
           END-IF.

      *    Calcul de puissance.
       2500-DO-POWER.
      
      *    Effectue un calcul de puissance et affiche le résultat.
           DISPLAY "PUISSANCE DEMANDÉE: " WS-DISPLAY-PREV  " ** " 
                   WS-DISPLAY-CURR
           COMPUTE WS-TEMP-RESULT = WS-PREVIOUS-RESULT ** 
      -            WS-CURRENT-NUM.
           MOVE WS-TEMP-RESULT TO WS-NUM-DISPLAY.
           DISPLAY "Puissance: " FUNCTION TRIM(WS-NUM-DISPLAY).
           MOVE WS-TEMP-RESULT TO WS-PREVIOUS-RESULT.


      *    Fin du programme.
       9900-TERMINATE.
      
      *    Affiche un message de fin du programme.
           DISPLAY "Programme terminé."
           STOP RUN.
