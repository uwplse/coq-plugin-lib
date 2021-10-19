import torch
import argparse
from gallina import GallinaTermParser
from models.prover import Prover
import string

term_parser = GallinaTermParser(caching=True)
sexp_cache = SexpCache('../../../../sexp_cache', readonly=True) # include this outside top directory

def get_opts():
	# TODO: need to update opts as we go
	parser = argparse.ArgumentParser()
    parser.add_argument('--path', type=str, default='model.pth')
    parser.add_argument('--beam_width', type=int, default=10) 
    parser.add_argument('--tac_grammar', type=str, default='tactics.ebnf')
    parser.add_argument('--term_embedding_dim', type=int, default=256)
    parser.add_argument('--embedding_dim', type=int, default=256, help='dimension of the grammar embeddings')
    parser.add_argument('--symbol_dim', type=int, default=256, help='dimension of the terminal/nonterminal symbol embeddings')
    parser.add_argument('--hidden_dim', type=int, default=256, help='dimension of the LSTM controller')
    parser.add_argument('--seed', type=int, default=0)
    parser.add_argument('--num_tactics', type=int, default=15025)
    parser.add_argument('--tac_vocab_file', type=str, default='token_vocab.pickle')
    parser.add_argument('--cutoff_len', type=int, default=30)
    opts = parser.parse_args()
    opts.device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    return opts

def import_model():
	opts = get_opts()
	model = Prover(opts)
    if opts.device.type == 'cpu':
        checkpoint = torch.load(opts.path, map_location='cpu')
    else:
        checkpoint = torch.load(opts.path)
    model.load_state_dict(checkpoint['state_dict'])
    model.to(opts.device)
    return model 

def filter_env(env):
    filtered_env = []
    for const in [const for const in env['constants'] if const['qualid'].startswith('SerTop')][-10:]:
        ast = sexp_cache[const['sexp']]
        filtered_env.append({'qualid': const['qualid'], 'ast': term_parser.parse(ast)})
    return filtered_env

def parse_goal(g): 
    goal = {'id': g['id'], 'text': g['type'], 'ast': term_parser.parse(g['sexp'])}
    local_context = []
    for i, h in enumerate(g['hypotheses']):
        for ident in h['idents']:
            local_context.append({'ident': ident, 'text': h['type'], 'ast': term_parser.parse(h['sexp'])})
    return local_context, goal['ast']

rem_punc = string.punctuation.replace('\'','').replace('_', '')
table = str.maketrans('', '', rem_punc)

def tokenize_text(raw_text):
    without_punc = raw_text.translate(table)
    words = without_punc.split()
    return words

def parse_script(script):
    prev_seq = []
    for tac in script:
        tac_words = tokenize_text(tac)
        prev_seq += tac_words

    return prev_seq