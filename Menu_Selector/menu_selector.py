from pyswip import Prolog

prolog = Prolog()

prolog.consult("person.pl")

c = list(prolog.query("findall(C,age(Names,C),Ages)"))
print(c)