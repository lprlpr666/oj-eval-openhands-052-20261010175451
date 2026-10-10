#include <iostream>
#include <vector>
#include <algorithm>
#include <cctype>
#include <string>
#include <sstream>
using namespace std;

struct Term {
    long long a; // coefficient
    int b, c, d; // exponents of x, sin, cos
    bool operator==(const Term& o) const { return b==o.b && c==o.c && d==o.d; }
};

struct Poly {
    vector<Term> t;
    void simplify() {
        sort(t.begin(), t.end(), [](const Term& x, const Term& y){
            if (x.b!=y.b) return x.b>y.b;
            if (x.c!=y.c) return x.c>y.c;
            return x.d>y.d;
        });
        vector<Term> res;
        for (auto &term: t) {
            if (term.a==0) continue;
            if (!res.empty() && term==res.back()) {
                res.back().a += term.a;
            } else {
                res.push_back(term);
            }
        }
        t.clear();
        for (auto &term: res) if (term.a!=0) t.push_back(term);
    }
    Poly() {}
    Poly(const Poly& o) = default;
    static Poly fromTerm(const Term& term) { Poly p; p.t.push_back(term); p.simplify(); return p; }
    static Poly constant(long long val) { return fromTerm({val,0,0,0}); }
    static Poly variableX() { return fromTerm({1,1,0,0}); }
    static Poly sinTerm(int exp) { return fromTerm({1,0,exp,0}); }
    static Poly cosTerm(int exp) { return fromTerm({1,0,0,exp}); }
    Poly operator+(const Poly& o) const {
        Poly ans; ans.t = t; ans.t.insert(ans.t.end(), o.t.begin(), o.t.end()); ans.simplify(); return ans; }
    Poly operator-(const Poly& o) const {
        Poly neg = o; for (auto &term: neg.t) term.a = -term.a; Poly ans; ans.t = t; ans.t.insert(ans.t.end(), neg.t.begin(), neg.t.end()); ans.simplify(); return ans; }
    Poly operator*(const Poly& o) const {
        Poly ans; for (auto &t1: t) for (auto &t2: o.t) {
            Term nt; nt.a = t1.a * t2.a; nt.b = t1.b + t2.b; nt.c = t1.c + t2.c; nt.d = t1.d + t2.d; ans.t.push_back(nt);
        }
        ans.simplify(); return ans; }
    Poly derivative() const {
        Poly ans; for (auto &term: t) {
            if (term.b != 0) {
                Term t1 = term; t1.a *= term.b; t1.b -= 1; ans = ans + Poly::fromTerm(t1);
            }
            if (term.c != 0) {
                Term t2 = term; t2.a *= term.c; t2.c -= 1; t2.d += 1; ans = ans + Poly::fromTerm(t2);
            }
            if (term.d != 0) {
                Term t3 = term; t3.a *= -term.d; t3.d -= 1; t3.c += 1; ans = ans + Poly::fromTerm(t3);
            }
        }
        ans.simplify(); return ans; }
};

struct Frac {
    Poly p, q; // p/q
    Frac() { p = Poly::constant(0); q = Poly::constant(1); }
    Frac(const Poly& _p, const Poly& _q): p(_p), q(_q) {}
    static Frac fromPoly(const Poly& poly) { return Frac(poly, Poly::constant(1)); }
    static Frac constant(long long val) { return Frac(Poly::constant(val), Poly::constant(1)); }
    Frac operator+(const Frac& o) const {
        Poly num = p * o.q + o.p * q;
        Poly den = q * o.q;
        return Frac(num, den);
    }
    Frac operator-(const Frac& o) const {
        Poly num = p * o.q - o.p * q;
        Poly den = q * o.q;
        return Frac(num, den);
    }
    Frac operator*(const Frac& o) const {
        return Frac(p * o.p, q * o.q);
    }
    Frac operator/(const Frac& o) const {
        return Frac(p * o.q, q * o.p);
    }
    Frac derivative() const {
        Poly num = p.derivative() * q - p * q.derivative();
        Poly den = q * q;
        return Frac(num, den);
    }
    void negate() { for (auto &term: p.t) term.a = -term.a; }
};

string s; int pos;
char peekChar() { return pos < (int)s.size() ? s[pos] : '\0'; }
char getChar() { return pos < (int)s.size() ? s[pos++] : '\0'; }
bool consume(char c) { if (peekChar()==c) { pos++; return true; } return false; }

Frac parseExpression();
Frac parseTerm();
Frac parseFactor();
Frac parsePrimary();

Frac parseExpression() {
    Frac left = parseTerm();
    while (true) {
        if (consume('+')) { left = left + parseTerm(); }
        else if (consume('-')) { left = left - parseTerm(); }
        else break;
    }
    return left;
}

Frac parseTerm() {
    Frac left = parseFactor();
    while (true) {
        if (consume('*')) { left = left * parseFactor(); }
        else if (consume('/')) { left = left / parseFactor(); }
        else break;
    }
    return left;
}

Frac parseFactor() {
    if (consume('+')) return parseFactor();
    if (consume('-')) { Frac f = parseFactor(); f.negate(); return f; }
    return parsePrimary();
}

long long parseNumber() {
    long long val = 0; bool has = false;
    while (isdigit(peekChar())) { has = true; val = val*10 + (getChar()-'0'); }
    return has ? val : 0;
}

Frac parsePrimary() {
    if (consume('(')) {
        Frac inside = parseExpression();
        consume(')');
        return inside;
    }
    if (isdigit(peekChar())) {
        long long num = parseNumber();
        return Frac::constant(num);
    }
    if (peekChar()=='x') {
        getChar();
        int exp = 1;
        if (consume('^')) exp = (int)parseNumber();
        Term t{1, exp, 0, 0};
        return Frac::fromPoly(Poly::fromTerm(t));
    }
    if (peekChar()=='s') {
        string name; for(int i=0;i<3;i++) name.push_back(getChar()); // sin
        int exp = 1; if (consume('^')) exp = (int)parseNumber();
        if (peekChar()=='x') getChar();
        Term t{1,0,exp,0};
        return Frac::fromPoly(Poly::fromTerm(t));
    }
    if (peekChar()=='c') {
        string name; for(int i=0;i<3;i++) name.push_back(getChar()); // cos
        int exp = 1; if (consume('^')) exp = (int)parseNumber();
        if (peekChar()=='x') getChar();
        Term t{1,0,0,exp};
        return Frac::fromPoly(Poly::fromTerm(t));
    }
    return Frac::constant(0);
}

string termToString(const Term& t) {
    if (t.a==0) return "";
    string res;
    long long a = t.a;
    bool hasVar = (t.b!=0 || t.c!=0 || t.d!=0);
    if (a==-1 && hasVar) res += "-";
    else if (a!=1 || !hasVar) res += to_string(a);
    if (t.b!=0) { res += "x"; if (t.b!=1) res += "^" + to_string(t.b); }
    if (t.c!=0) { res += "sin"; if (t.c!=1) res += "^" + to_string(t.c); res += "x"; }
    if (t.d!=0) { res += "cos"; if (t.d!=1) res += "^" + to_string(t.d); res += "x"; }
    return res;
}

string polyToString(const Poly& p) {
    if (p.t.empty()) return "0";
    string out;
    for (size_t i=0;i<p.t.size();++i) {
        const Term& t = p.t[i];
        string ts = termToString(t);
        if (ts.empty()) continue;
        if (i==0) out += ts;
        else {
            if (t.a>0) out += "+" + ts;
            else out += ts; // ts includes '-'
        }
    }
    if (out.empty()) out = "0";
    return out;
}

string fracToString(const Frac& f) {
    Poly num = f.p; Poly den = f.q;
    bool denIsOne = (den.t.size()==1 && den.t[0].a==1 && den.t[0].b==0 && den.t[0].c==0 && den.t[0].d==0);
    string numStr = polyToString(num);
    string denStr = polyToString(den);
    if (denIsOne) return numStr;
    bool numMulti = (num.t.size()>1);
    bool denMulti = (den.t.size()>1);
    if (numMulti) numStr = "(" + numStr + ")";
    if (denMulti) denStr = "(" + denStr + ")";
    return numStr + "/" + denStr;
}

int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    string input;
    if(!(cin>>input)) return 0;
    // Insert explicit multiplication where needed (e.g., 3x -> 3*x, sinxcosx -> sinx*cosx)
    string processed;
    for (size_t i = 0; i < input.size(); ++i) {
        processed.push_back(input[i]);
        if (i + 1 < input.size()) {
            char cur = input[i];
            char nxt = input[i+1];
            bool curIsPart = (isalnum(cur) || cur == ')');
            bool nxtIsPart = (nxt == 'x' || nxt == 's' || nxt == 'c' || nxt == '(');
            if (curIsPart && nxtIsPart) {
                // avoid inserting inside identifiers like "sin" or "cos"
                bool isSin = (i>=2 && input[i-2]=='s' && input[i-1]=='i' && cur=='n' && nxt=='x');
            bool isCos = (i>=2 && input[i-2]=='c' && input[i-1]=='o' && cur=='s' && nxt=='x');
            if (!isSin && !isCos) {
                    processed.push_back('*');
                }
            }
        }
    }
    cerr << "Processed: " << processed << "\n";
    s = processed; pos = 0;
    Frac expr = parseExpression();
    string out1 = fracToString(expr);
    Frac deriv = expr.derivative();
    string out2 = fracToString(deriv);
    cout << out1 << "\n" << out2 << "\n";
    return 0;
}
