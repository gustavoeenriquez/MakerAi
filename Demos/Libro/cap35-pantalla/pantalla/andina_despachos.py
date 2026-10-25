#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Andina Despachos: el programa de escritorio (ficticio) de la transportadora de Café La Ladera.
No tiene API: la guía se registra a mano. Es la app que Lucía opera en el capítulo 35.
Cada guía registrada se anota en ANDINA_LOG (por defecto /tmp/andina_guias.txt) para comprobar
qué escribió el modelo. Café La Ladera: empresa, productos y precios ficticios."""
import datetime
import hashlib
import os
import tkinter as tk

LOG = os.environ.get("ANDINA_LOG", "/tmp/andina_guias.txt")
CAMPOS = [("Destinatario", "destinatario"), ("Ciudad", "ciudad"), ("Dirección", "direccion"),
          ("Peso (kg)", "peso"), ("Contenido", "contenido")]

root = tk.Tk()
root.title("Andina Despachos 2.3")
root.geometry("1280x800+0+0")
root.configure(bg="#eef1f5")

tk.Frame(root, bg="#1f3a5f", height=90).pack(fill="x")
tk.Label(root, text="ANDINA DESPACHOS  ·  Registro de guías", font=("DejaVu Sans", 24, "bold"),
         bg="#1f3a5f", fg="white").place(x=40, y=24)

form = tk.Frame(root, bg="#eef1f5")
form.place(x=80, y=140)
entradas = {}
for fila, (rotulo, clave) in enumerate(CAMPOS):
    tk.Label(form, text=rotulo + ":", font=("DejaVu Sans", 18), bg="#eef1f5", fg="#222",
             anchor="e", width=14).grid(row=fila, column=0, padx=(0, 16), pady=14, sticky="e")
    e = tk.Entry(form, font=("DejaVu Sans", 18), width=38, bg="white", fg="#111",
                 relief="solid", bd=1, highlightthickness=2, highlightcolor="#1f3a5f")
    e.grid(row=fila, column=1, pady=14, ipady=6)
    entradas[clave] = e

resultado = tk.Label(root, text="", font=("DejaVu Sans", 20, "bold"), bg="#eef1f5", fg="#1b7a3a")
resultado.place(x=80, y=640)


def registrar():
    datos = {k: e.get().strip() for k, e in entradas.items()}
    vacios = [r for r, k in CAMPOS if not datos[k]]
    if vacios:
        resultado.config(text="Faltan campos: " + ", ".join(vacios), fg="#b00020")
        return
    base = "|".join(datos[k] for _, k in CAMPOS)
    numero = "AND-%s-%04d" % (datetime.date.today().strftime("%Y%m%d"),
                               int(hashlib.sha1(base.encode("utf-8")).hexdigest(), 16) % 10000)
    resultado.config(text="Guía %s registrada para %s (%s)" % (numero, datos["destinatario"], datos["ciudad"]),
                     fg="#1b7a3a")
    with open(LOG, "a", encoding="utf-8") as f:
        f.write("%s\t%s\t%s\n" % (datetime.datetime.now().isoformat(timespec="seconds"), numero, base))


tk.Button(root, text="Registrar guía", font=("DejaVu Sans", 18, "bold"), bg="#1f3a5f", fg="white",
          activebackground="#2d5285", activeforeground="white", padx=24, pady=10,
          command=registrar).place(x=340, y=560)

root.mainloop()
