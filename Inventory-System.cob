       IDENTIFICATION DIVISION.
       PROGRAM-ID. Inventory-System.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Product-Record.
           05 Product-ID           PIC 9(5).
           05 Product-Name         PIC X(30).
           05 Product-Price        PIC 9(5).
           05 Stock-Quantity       PIC 9(5).
       
       01 Product-List. 
           05 Product-Entry OCCURS 100 TIMES.
              10 ID-List           PIC 9(5).
              10 Name-List         PIC X(30).
              10 Price-List        PIC 9(5).
              10 Quantity-List     PIC 9(5).
       
       01 Choice                   PIC X(1).
       01 Product-Count            PIC 9(5) VALUE 0.
       01 ID-Temp                  PIC 9(5).
       01 New-Stock                PIC 9(5).
       
       01 I PIC 9(3) VALUE 0.
       
       PROCEDURE DIVISION.
       
       Main-Menu.
       DISPLAY "1. Registrar nuevo producto"
       DISPLAY "2. Generar informe de inventario"
       DISPLAY "3. Actualizar stock"
       DISPLAY "4. Salir"
       ACCEPT Choice
       PERFORM Action-Menu.
       
       Action-Menu.
       EVALUATE Choice
          WHEN '1' 
             PERFORM Register-Product
          WHEN '2' 
            PERFORM Generate-Inventory-Report
          WHEN '3' 
            PERFORM Update-Stock
          WHEN '4' 
             DISPLAY "Saliendo del sistema." 
             STOP RUN
          WHEN OTHER 
             DISPLAY "Opción no válida. Inténtelo de nuevo." 
             PERFORM Main-Menu
       END-EVALUATE.
       
       Register-Product.
       DISPLAY "Registro de nuevo producto:"
       ADD 1 TO Product-Count
       MOVE Product-Count TO Product-ID
       DISPLAY "Ingrese el nombre del producto:"
       ACCEPT Product-Name
       DISPLAY "Ingrese el precio del producto:"
       ACCEPT Product-Price
       DISPLAY "Ingrese el stock del producto:"
       ACCEPT Stock-Quantity
       DISPLAY "Producto registrado con éxito."
       
       MOVE Product-ID TO Product-Entry(Product-Count)(1:5).
       MOVE Product-Name TO Product-Entry(Product-Count)(6:30).
       MOVE Product-Price TO Product-Entry(Product-Count)(36:5).
       MOVE Stock-Quantity TO Product-Entry(Product-Count)(41:5).
       
       PERFORM Main-Menu.
       
       Generate-Inventory-Report.
       DISPLAY "Informe de inventario:"
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > Product-Count
           DISPLAY "ID: " Product-Entry(I)(1:5)
           DISPLAY "Nombre: " Product-Entry(I)(6:30)
           DISPLAY "Precio: $" Product-Entry(I)(36:5)
           DISPLAY "Stock: " Product-Entry(I)(41:5)
       END-PERFORM

       PERFORM Main-Menu.

       Update-Stock.
       DISPLAY "Actualización de stock:"
       DISPLAY "Ingrese el ID del producto:"
       ACCEPT ID-Temp
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > Product-Count
           IF Product-Entry(I)(1:5) = ID-Temp
              DISPLAY "Ingrese el nuevo stock del producto:"
              ACCEPT New-Stock
              MOVE New-Stock TO Product-Entry(I)(41:5)
              DISPLAY "Stock actualizado con éxito!"
           END-IF
       END-PERFORM
       DISPLAY "Producto no encontrado. Stock no actualizado."

       PERFORM Main-Menu.
