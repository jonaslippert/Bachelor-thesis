fof(m_,hypothesis,$true).
fof(m_,hypothesis,$true).
fof(m_,hypothesis,$true).
fof(m_,hypothesis,( ! [W0] : ( ! [W1] : ( ! [W2] : (((aCollectionOfArrows(W0) & aCollectionOfArrows(W1)) & aArrow(W2)) => ((W0 = W1) <=> (sdtbsziznzndt(W2,W0) <=> sdtbsziznzndt(W2,W1)))))))).
fof(m_,hypothesis,( ! [W0] : (aArrow(W0) => aArrow(szslbdtrb(W0))))).
fof(m_,hypothesis,( ! [W0] : (aArrow(W0) => aArrow(sztlbdtrb(W0))))).
fof(m_,hypothesis,( ! [W0] : ( ! [W1] : ((aArrow(W0) & aArrow(W1)) => aArrow(sdtbszmzczizrzcdt(W1,W0)))))).
fof(m_,hypothesis,( ! [W0] : (aCategory(W0) <=> (aCollectionOfArrows(W0) & ((( ! [W1] : ((aArrow(W1) & sdtbsziznzndt(W1,W0)) => (((((sdtbsziznzndt(szslbdtrb(W1),W0) & sdtbsziznzndt(sztlbdtrb(W1),W0)) & (sztlbdtrb(szslbdtrb(W1)) = szslbdtrb(W1))) & (szslbdtrb(sztlbdtrb(W1)) = sztlbdtrb(W1))) & (W1 = sdtbszmzczizrzcdt(W1,szslbdtrb(W1)))) & (W1 = sdtbszmzczizrzcdt(sztlbdtrb(W1),W1))))) & ( ! [W1] : ( ! [W2] : (((aArrow(W1) & aArrow(W2)) & (sdtbsziznzndt(W1,W0) & sdtbsziznzndt(W2,W0))) => ((sztlbdtrb(W1) = szslbdtrb(W2)) => (aArrow(sdtbszmzczizrzcdt(W2,W1)) & (sdtbsziznzndt(sdtbszmzczizrzcdt(W2,W1),W0) & ( ! [W3] : ((aArrow(W3) & (sdtbsziznzndt(W3,W0) & (sdtbszmzczizrzcdt(W2,W1) = W3))) => (sdtbszmzczizrzcdt(W2,W1) = W3)))))))))) & ( ! [W1] : ( ! [W2] : ( ! [W3] : ((((aArrow(W1) & aArrow(W2)) & aArrow(W3)) & ((((sdtbsziznzndt(W1,W0) & sdtbsziznzndt(W2,W0)) & sdtbsziznzndt(W3,W0)) & (sztlbdtrb(W1) = szslbdtrb(W2))) & (sztlbdtrb(W2) = szslbdtrb(W3)))) => (sdtbszmzczizrzcdt(sdtbszmzczizrzcdt(W3,W2),W1) = sdtbszmzczizrzcdt(W3,sdtbszmzczizrzcdt(W2,W1)))))))))))).
fof(m_,hypothesis,( ! [W0] : (aSset(W0) <=> (aSet(W0) & ( ? [W1] : (aSet(W1) & aElementOf(W0,W1))))))).
fof(m_,hypothesis,$true).
fof(m_,hypothesis,( ! [W0] : (aFunction(W0) => aSet(szCzozdlpdtrp(W0))))).
fof(m_,hypothesis,( ! [W0] : (aFunction(W0) => ( ! [W1] : (aElementOf(W1,szDzozmlpdtrp(W0)) => aElementOf(sdtlpdtrp(W0,W1),szCzozdlpdtrp(W0))))))).
fof(mext,hypothesis,( ! [W0] : ( ! [W1] : ((((aFunction(W0) & aFunction(W1)) & (szDzozmlpdtrp(W0) = szDzozmlpdtrp(W1))) & (szCzozdlpdtrp(W0) = szCzozdlpdtrp(W1))) => (( ! [W2] : (aElementOf(W2,szDzozmlpdtrp(W0)) => (sdtlpdtrp(W0,W2) = sdtlpdtrp(W1,W2)))) => (W0 = W1)))))).
fof(m_,hypothesis,( ! [W0] : ( ! [W1] : ((aFunction(W0) & aFunction(W1)) => ((szCzozdlpdtrp(W0) = szDzozmlpdtrp(W1)) => ((aFunction(sdtbszczizrzcdt(W1,W0)) & (((szDzozmlpdtrp(sdtbszczizrzcdt(W1,W0)) = szDzozmlpdtrp(W0)) & (szCzozdlpdtrp(sdtbszczizrzcdt(W1,W0)) = szCzozdlpdtrp(W1))) & ( ! [W2] : (aElementOf(W2,szDzozmlpdtrp(W0)) => (sdtlpdtrp(sdtbszczizrzcdt(W1,W0),W2) = sdtlpdtrp(W1,sdtlpdtrp(W0,W2))))))) & ( ! [W2] : ((aFunction(W2) & (((szDzozmlpdtrp(W2) = szDzozmlpdtrp(W0)) & (szCzozdlpdtrp(W2) = szCzozdlpdtrp(W1))) & ( ! [W3] : (aElementOf(W3,szDzozmlpdtrp(W0)) => (sdtlpdtrp(W2,W3) = sdtlpdtrp(W1,sdtlpdtrp(W0,W3))))))) => (W2 = sdtbszczizrzcdt(W1,W0)))))))))).
fof(m_,hypothesis,( ! [W0] : (aFunction(W0) => aArrow(W0)))).
fof(m_,hypothesis,( ! [W0] : (aFunction(W0) => (aFunction(szslbdtrb(W0)) & ((szDzozmlpdtrp(szslbdtrb(W0)) = szDzozmlpdtrp(W0)) & (szDzozmlpdtrp(W0) = szCzozdlpdtrp(szslbdtrb(W0)))))))).
fof(m_,hypothesis,( ! [W0] : (aFunction(W0) => ( ! [W1] : (aElementOf(W1,szDzozmlpdtrp(W0)) => (sdtlpdtrp(szslbdtrb(W0),W1) = W1)))))).
fof(m_,hypothesis,( ! [W0] : (aFunction(W0) => (aFunction(sztlbdtrb(W0)) & ((szDzozmlpdtrp(sztlbdtrb(W0)) = szCzozdlpdtrp(W0)) & (szCzozdlpdtrp(W0) = szCzozdlpdtrp(sztlbdtrb(W0)))))))).
fof(m_,hypothesis,( ! [W0] : (aFunction(W0) => ( ! [W1] : (aElementOf(W1,szCzozdlpdtrp(W0)) => (sdtlpdtrp(sztlbdtrb(W0),W1) = W1)))))).
fof(m_,hypothesis,((aClass(szSzEzT) & ( ! [W0] : (aElementOf(W0,szSzEzT) <=> ((aFunction(W0) & (aSset(szDzozmlpdtrp(W0)) & aSset(szCzozdlpdtrp(W0)))) & isSetSized(W0))))) & ( ! [W0] : ((aClass(W0) & ( ! [W1] : (aElementOf(W1,W0) <=> ((aFunction(W1) & (aSset(szDzozmlpdtrp(W1)) & aSset(szCzozdlpdtrp(W1)))) & isSetSized(W1))))) => (W0 = szSzEzT))))).
fof(m_,hypothesis,aSet(szSzEzT)).
fof(m_,hypothesis,( ! [W0] : (aFunction(W0) => ((aSset(szDzozmlpdtrp(W0)) & aSset(szCzozdlpdtrp(W0))) => isSetSized(W0))))).
fof(m_,hypothesis,aCollectionOfArrows(szSzEzT)).
fof(m_,hypothesis,( ! [W0] : (aArrow(W0) => (sdtbsziznzndt(W0,szSzEzT) <=> aElementOf(W0,szSzEzT))))).
fof(m_,hypothesis,( ! [W0] : ((aArrow(W0) & sdtbsziznzndt(W0,szSzEzT)) => ((szslbdtrb(W0) = szDzozmlpdtrp(W0)) & (sztlbdtrb(W0) = szCzozdlpdtrp(W0)))))).
fof(m_,hypothesis,( ! [W0] : ( ! [W1] : (((aElementOf(W0,szSzEzT) & aElementOf(W1,szSzEzT)) & (szCzozdlpdtrp(W0) = szDzozmlpdtrp(W1))) => (sdtbszczizrzcdt(W1,W0) = sdtbszmzczizrzcdt(W1,W0)))))).
fof(m_,hypothesis,$true).
fof(m_,hypothesis,( ! [W0] : ( ! [W1] : ((aSet(W0) & aSet(W1)) => ( ! [W2] : ((aFunction(W2) & ((szDzozmlpdtrp(W2) = W0) & (szCzozdlpdtrp(W2) = W1))) => ( ! [W3] : ((aFunction(W3) & ((szDzozmlpdtrp(W3) = W1) & ( ! [W4] : (aElementOf(W4,W1) => aElementOf(sdtlpdtrp(W3,W4),W0))))) => (( ! [W4] : (aElementOf(W4,szDzozmlpdtrp(W3)) => (sdtlpdtrp(W2,sdtlpdtrp(W3,W4)) = W4))) => (( ! [W4] : (aElementOf(W4,szDzozmlpdtrp(W2)) => (sdtlpdtrp(W3,sdtlpdtrp(W2,W4)) = W4))) => aBijectionBetweenAnd(W2,W0,W1))))))))))).
fof(m_,hypothesis,$true).
fof(m_,hypothesis,( ! [W0] : (aFunctor(W0) => ( ! [W1] : (aArrow(W1) => aArrow(sdtlbdtrb(W0,W1))))))).
fof(m_,hypothesis,( ! [W0] : ( ! [W1] : ((aCategory(W0) & aCategory(W1)) => ( ! [W2] : (aFunctorFromTo(W2,W0,W1) <=> (aFunctor(W2) & (( ! [W3] : ((aArrow(W3) & sdtbsziznzndt(W3,W0)) => ((sdtbsziznzndt(sdtlbdtrb(W2,W3),W1) & (sdtlbdtrb(W2,szslbdtrb(W3)) = szslbdtrb(sdtlbdtrb(W2,W3)))) & (sdtlbdtrb(W2,sztlbdtrb(W3)) = sztlbdtrb(sdtlbdtrb(W2,W3)))))) & ( ! [W3] : ( ! [W4] : (((aArrow(W3) & aArrow(W4)) & ((sdtbsziznzndt(W3,W0) & sdtbsziznzndt(W4,W0)) & (sztlbdtrb(W3) = szslbdtrb(W4)))) => (sdtlbdtrb(W2,sdtbszmzczizrzcdt(W4,W3)) = sdtbszmzczizrzcdt(sdtlbdtrb(W2,W4),sdtlbdtrb(W2,W3)))))))))))))).
fof(m_,hypothesis,( ! [W0] : (aCategory(W0) => ( ! [W1] : ( ! [W2] : (((aArrow(W1) & sdtbsziznzndt(W1,W0)) & (aArrow(W2) & sdtbsziznzndt(W2,W0))) => (aCollectionOfArrows(szHzozmlbdtcmdtcmdtrb(W0,W1,W2)) & ( ! [W3] : ((aArrow(W3) & sdtbsziznzndt(W3,szHzozmlbdtcmdtcmdtrb(W0,W1,W2))) => sdtbsziznzndt(W3,W0)))))))))).
fof(m_,hypothesis,( ! [W0] : (aCategory(W0) => ( ! [W1] : ( ! [W2] : (((aArrow(W1) & sdtbsziznzndt(W1,W0)) & (aArrow(W2) & sdtbsziznzndt(W2,W0))) => ( ! [W3] : (aArrow(W3) => (sdtbsziznzndt(W3,szHzozmlbdtcmdtcmdtrb(W0,W1,W2)) <=> ((szslbdtrb(W3) = W1) & (sztlbdtrb(W3) = W2))))))))))).
fof(m_,hypothesis,( ! [W0] : (aLocallySmallCategory(W0) <=> (aCategory(W0) & ( ! [W1] : ( ! [W2] : (((aArrow(W1) & aArrow(W2)) & (sdtbsziznzndt(W1,W0) & sdtbsziznzndt(W2,W0))) => aElementOf(szHzozmlbdtcmdtcmdtrb(W0,W1,W2),szSzEzT)))))))).
fof(m_,hypothesis,( ! [W0] : (aLocallySmallCategory(W0) => ( ! [W1] : ( ! [W2] : (((aArrow(W1) & sdtbsziznzndt(W1,W0)) & (aArrow(W2) & sdtbsziznzndt(W2,W0))) => ( ! [W3] : (aArrow(W3) => (aElementOf(W3,szDzozmlpdtrp(szHzozmlbdtcmdtcmdtrb(W0,W1,W2))) <=> sdtbsziznzndt(W3,szHzozmlbdtcmdtcmdtrb(W0,W1,W2))))))))))).
fof(m_,hypothesis,( ! [W0] : (aLocallySmallCategory(W0) => ( ! [W1] : ( ! [W2] : (((aArrow(W1) & sdtbsziznzndt(W1,W0)) & (aArrow(W2) & sdtbsziznzndt(W2,W0))) => (((szDzozmlpdtrp(szHzozmlbdtcmdtcmdtrb(W0,W1,W2)) = szHzozmlbdtcmdtcmdtrb(W0,W1,szslbdtrb(W2))) & (szCzozdlpdtrp(szHzozmlbdtcmdtcmdtrb(W0,W1,W2)) = szHzozmlbdtcmdtcmdtrb(W0,W1,sztlbdtrb(W2)))) & ( ! [W3] : ((aArrow(W3) & aElementOf(W3,szHzozmlbdtcmdtcmdtrb(W0,W1,szslbdtrb(W2)))) => (sdtlpdtrp(szHzozmlbdtcmdtcmdtrb(W0,W1,W2),W3) = sdtbszmzczizrzcdt(W2,W3))))))))))).
fof(m_,hypothesis,( ! [W0] : (aLocallySmallCategory(W0) => ( ! [W1] : ( ! [W2] : (((aArrow(W1) & sdtbsziznzndt(W1,W0)) & (aArrow(W2) & sdtbsziznzndt(W2,W0))) => ( ! [W3] : (aElementOf(W3,szDzozmlpdtrp(szHzozmlbdtcmdtcmdtrb(W0,W1,W2))) => aArrow(W3))))))))).
fof(m_,hypothesis,( ! [W0] : (aLocallySmallCategory(W0) => ( ! [W1] : ((aArrow(W1) & sdtbsziznzndt(W1,W0)) => ((aFunctor(szHzozmzFlbdtcmdtrb(W0,W1)) & ( ! [W2] : ((aArrow(W2) & sdtbsziznzndt(W2,W0)) => (sdtlbdtrb(szHzozmzFlbdtcmdtrb(W0,W1),W2) = szHzozmlbdtcmdtcmdtrb(W0,W1,W2))))) & ( ! [W2] : ((aFunctor(W2) & ( ! [W3] : ((aArrow(W3) & sdtbsziznzndt(W3,W0)) => (sdtlbdtrb(W2,W3) = szHzozmlbdtcmdtcmdtrb(W0,W1,W3))))) => (W2 = szHzozmzFlbdtcmdtrb(W0,W1)))))))))).
fof(m__,conjecture,$false).
