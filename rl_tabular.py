#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
rl_tabular.py
------------


"""

__author__ = 'juliowaissman'


import random


def mejor_accion(mdpsim, q, s):
    return max(mdpsim.a_legales(s), key=lambda accion: q[(s, accion)])


def sarsa0(mdpsim, descuento, egreedy, alpha, max_episodios, max_pasos):
    # Inicializa
    q = {(s, a): 0 for s in mdpsim.estados for a in mdpsim.a_legales(s)}

    for _ in range(max_episodios):             # Por cada episodio

        s = mdpsim.estado_inicial()            # Estado inicial del episodio
        # Política epsilon-greedy
        if random.random() < 1 - egreedy:
            a = mejor_accion(mdpsim, q, s)
        else:
            a = random.choice(mdpsim.a_legales(s))

        for _ in range(max_pasos):             # Por cada paso en el episodio

            # Simulación
            sp, r = mdpsim.transicion(s, a)

            if len(mdpsim.a_legales(sp)) == 0:
                q[(s, a)] += alpha * (r - q[(s, a)])
                break

            # Política epsilon-greedy
            if random.random() < 1 - egreedy:
                ap = mejor_accion(mdpsim, q, sp)
            else:
                ap = random.choice(mdpsim.a_legales(sp))

            # Aprendizaje por refuerzo
            inc_q = r + descuento * q[(sp, ap)] - q[(s, a)]
            q[(s, a)] += alpha * inc_q

            # Actualización y criterio de fin de episodio
            s = sp
            a = ap

    # Regresa la política
    return {s: mejor_accion(mdpsim, q, s) for s in mdpsim.estados}


def qlearning0(mdpsim, descuento, egreedy, alpha, max_episodios, max_pasos):

    # Inicializa
    q = {(s, a): 0 for s in mdpsim.estados for a in mdpsim.a_legales(s)}

    for _ in range(max_episodios):             # Por cada episodio
        s = mdpsim.estado_inicial()            # Estado inicial del episodio
        for _ in range(max_pasos):             # Por cada paso en el episodio

            # Política epsilon-greedy
            if random.random() < 1 - egreedy:
                a = mejor_accion(mdpsim, q, s)
            else:
                a = random.choice(mdpsim.a_legales(s))

            # Simulación
            sp, r = mdpsim.transicion(s, a)

            # Aprendizaje por refuerzo
            inc_q = r + descuento * max(q[(sp, accion)] for accion in mdpsim.a_legales(sp)) - q[(s, a)]
            q[(s, a)] += alpha * inc_q

            # Actualización y criterio de fin de episodio
            s = sp
            if mdpsim.terminal(s):
                break

    # Regresa la política
    return {s: mejor_accion(mdpsim, q, s) for s in mdpsim.estados}


def qlearning(mdpsim, descuento, egreedy, alpha, lammbda, max_episodios, max_pasos):

    q = {(s, a): 0 for s in mdpsim.estados for a in mdpsim.a_legales(s)}
    e = {(s, a): 0 for s in mdpsim.estados for a in mdpsim.a_legales(s)}

    for _ in range(max_episodios):
        s = mdpsim.estado_inicial()
        a_star = mejor_accion(mdpsim, q, s)
        a = a_star if random.random() < 1 - egreedy else random.choice(mdpsim.a_legales(s))

        for _ in range(max_pasos):
            sp, r = mdpsim.transicion(s, a)

            ap_star = mejor_accion(mdpsim, q, sp)
            ap = ap_star if random.random() < 1 - egreedy else random.choice(mdpsim.a_legales(sp))

            inc_q = r + descuento * max(q[(sp, accion)] for accion in mdpsim.a_legales(sp)) - q[(s, a)]
            e[(s, a)] = 1

            for estado in mdpsim.estados:
                for accion in mdpsim.a_legales(estado):
                    q[(estado, accion)] += alpha * inc_q * e[(estado, accion)]
                    e[(estado, accion)] = e[(estado, accion)] * lammbda * descuento if ap == ap_star else 0
            if mdpsim.terminal(sp):
                break
            s = sp
            a = ap

    return {s: mejor_accion(mdpsim, q, s) for s in mdpsim.estados}


if __name__ == '__main__':

    # El ejemplo del inventario para un caso muy simple que es fácil hacer a mano.
    from inventario import InventarioSim
    modelo = InventarioSim(m=10, b=0, max_dem=10, a_max=10, primero=True, c=(0, 1, 1, 0), gu=3)

    pi = sarsa0(modelo, descuento=0.99, egreedy=0.1, alpha=0.1, max_episodios=100000, max_pasos=50)
    print pi

    #pi = qlearning0(modelo, descuento=0.99, egreedy=0.05, alpha=0.4, max_episodios=10000, max_pasos=40)
    #print pi
    #
    #pi = qlearning(modelo, descuento=0.99, egreedy=0.05, alpha=0.4, lammbda=0.9, max_episodios=1000, max_pasos=40)
    #print pi
