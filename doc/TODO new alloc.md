get_token
when token_type is 'i', identifier is token_value

maybe parse_value
[token_type], 'i'

IMPORT_IDENTIFIER is a ptr already?
EXPORT_IDENTIFIER


store identifier:

OK add_frame_symbol
    OK FRAME_IDENTIFIER - check all uses
    OK deleting FRAME, also delete the identifier
    OK no longer free after calling

OK set_global_symbol_address, sometimes
OK set_global_symbol_type, sometimes
    OK no longer free after calling
    OK accept null identifier

OK add_fixup
    OK no longer free after calling
    OK accept null identifier - because find_global_symbol and add_global_symbol will accept it

OK add_global_symbol
    OK deleting GLOBAL, also delete the identifier - we never delete GLOBAL
    OK no longer free after calling
    OK accept null identifier

OK GLOBAL_IDENTIFIER
    OK check all uses
    OK change to GLOBAL_IDENTIFIER_PTR
    OK all uses need to accept null identifier


pass identifier:

OK find_frame_symbol

OK find_global_symbol
    OK accept null identifier
