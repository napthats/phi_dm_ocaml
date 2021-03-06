OCAMLC=ocamlfind ocamlc -w A -linkpkg -package extlib -thread
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

# prog1 should be compiled to bytecode, and is composed of three
# units: mod1, mod2 and mod3.

# The list of object files for prog1
PROG1_OBJS=tcp_server.cma phi_map_data.cmo dm_message.cmo item.cmo spell.cmo phi_map.cmo protocol.cmo chara.cmo client_manager.cmo chara_id.cmo chara_name_cache.cmo chara_status.cmo combat.cmo player_character_db.cmo player_character.cmo non_player_character.cmo chara_data.cmo chara_manager.cmo clock.cmo phi_dm.cmo

a.out: $(PROG1_OBJS)
	$(OCAMLC) -o a.out $(OCAMLFLAGS) $(PROG1_OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f a.out
	rm -f *.cm[iox]

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
