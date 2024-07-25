implement:

OK add_or_find_current_child_symbol(identifier) -> child
OK add_or_find_child_symbol(global, identifier) -> child
OK set_child_symbol_address(child, address)



change:

OK parse_value - needs to accept 'd'
parse_value - needs to accept 'i' 'd'
OK parse_symbol - that's the label +1 = label: - modified to accept +1 = .label: as well
OK parse_dir_symbol - .SYMBOL identifier xyz - done since xyz can only be number or char
OK parse_dir_import_export .IMPORT/.EXPORT identifier xyz - done, no value



need to handle children:

OK add_fixup needs to accept both global symbols and child symbol
OK process_fixups needs to process also child fixups

OK report_global_symbol_error
OK report_global_fixup_error
OK print_symbol_identifier

OK remember last global symbol that has an address set, from parse_symbol only I think
OK handle situation when there is no last global symbol yet (i.e. dot-identifier before first identifier)



tests:
OK - child symbol before first global symbol, should fail
OK - create child after second global, check that it is assigned to the second global
OK - child with with same name as a child under different global
OK - access through dot
 - access through parent.dot, from inside the parent and outside
OK - +1 = .child:
OK - .child + 1, .child - 1
 - +1, -1 with parent.child
 - check changes for other scenarios
