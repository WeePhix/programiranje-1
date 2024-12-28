set_option autoImplicit false

/------------------------------------------------------------------------------
 ## Naravna števila

 Definirajte funkcijo, ki _rekurzivno_ (torej naivno in ne direktno s formulo,
 ki jo boste morali dokazati) sešteje prvih `n` naravnih števil, ter
 dokažite, da zanjo velja znana enakost (najprej v obliki, ki ne zahteva
 deljenja, nato pa še v običajni obliki).
------------------------------------------------------------------------------/

def vsota_prvih : Nat → Nat :=
  fun
  | Nat.zero => 0
  | Nat.succ n => n + 1 + vsota_prvih n

theorem dva_je_ena_plus_ena: 2 = 1 + 1 := by rw [← Nat.two_mul, Nat.mul_comm, Nat.one_mul]

theorem gauss : (n : Nat) → 2 * vsota_prvih n = n * (n + 1) := by
  intro n
  induction n with
  | zero => simp[vsota_prvih]
  | succ m ih =>
    simp [vsota_prvih]
    rw [Nat.mul_add, ih, ← Nat.add_mul, Nat.mul_comm, ← Nat.add_comm]
    rw [dva_je_ena_plus_ena]
    calc (1 + m) * (1 + 1 + m)
    _ = (1 + m) * (1 + m + 1) := by rw [Nat.add_assoc, Nat.add_assoc, Nat.add_comm]

theorem cisto_pravi_gauss : (n : Nat) → vsota_prvih n = (n * (n + 1)) / 2 := by
  intro n
  calc vsota_prvih n
  _ = 2 * vsota_prvih n / 2 := by simp
  _ = n * (n + 1 ) / 2 := by simp [gauss]

/------------------------------------------------------------------------------
 ## Vektorji

 Definirajmo vektorje podobno kot na predavanjih, le da namesto svojih naravnih
 števil uporabimo vgrajena. Da se tipi ujamejo, funkcijo stikanja napišemo s
 pomočjo taktik.

 Napišite funkcijo `obrni`, ki vrne na glavo obrnjen vektor, ter funkciji
 `glava` in `rep`, ki varno vrneta glavo in rep _nepraznega_ seznama.
------------------------------------------------------------------------------/

inductive Vektor : Type → Nat → Type where
  | prazen : {A : Type} → Vektor A 0
  | sestavljen : {A : Type} → {n : Nat} → A → Vektor A n → Vektor A (n + 1)
deriving Repr

def stakni : {A : Type} → {m n : Nat} → Vektor A m → Vektor A n → Vektor A (m + n) :=
  fun xs ys => match xs with
  | .prazen => by rw [Nat.add_comm]; exact ys
  | .sestavljen x xs' => by rw [Nat.add_right_comm]; exact Vektor.sestavljen x (stakni xs' ys)

def obrni : {A : Type} → {n : Nat} → Vektor A n → Vektor A n := fun
  | Vektor.prazen => Vektor.prazen
  | Vektor.sestavljen x xs => stakni (obrni xs) (Vektor.sestavljen x Vektor.prazen)

def glava : {A : Type} → {n : Nat} → Vektor A n → Option A := fun
  | Vektor.prazen => Option.none
  | Vektor.sestavljen x _ => Option.some x

def rep : {A : Type} → {n : Nat} → Vektor A n → Option (Vektor A (n - 1)) := fun
  | Vektor.prazen => Option.none
  | Vektor.sestavljen _ xs => Option.some xs

/------------------------------------------------------------------------------
 ## Predikatni račun

 Dokažite spodnje tri trditve. Zadnja je _paradoks pivca_, ki pravi:
   "V vsaki neprazni gostilni obstaja gost, za katerega velja,
   da če pije on, pijejo vsi v gostilni."
 Za dokaz potrebujete klasično logiko, torej nekaj iz modula `Classical`.
------------------------------------------------------------------------------/

theorem forall_implies : {A : Type} → {P Q : A → Prop} →
  (∀ x, (P x → Q x)) → (∀ x, P x) → (∀ x, Q x) := by
  intro A P Q lh rh x
  apply lh
  apply rh

theorem forall_implies' : {A : Type} → {P : Prop} → {Q : A → Prop} →
  (∀ x, (P → Q x)) ↔ (P → ∀ x, Q x) := by
  intro A P Q
  constructor
  case mp =>
    intro R S x
    apply R
    assumption
  case mpr =>
    intro R x P
    apply R
    apply P


theorem paradoks_pivca :
  {G : Type} → {P : G → Prop} →
  (g : G) →  -- (g : G) pove, da je v gostilni vsaj en gost
  ∃ (p : G), (P p → ∀ (x : G), P x) := by
  intro G P g
  apply Classical.byCases
  case hpq => -- vsi pijejo
    intro p
    exists g -- naj je g tisti gost, ki odloca o tem ali pijejo vsi ali ne
  case hnpq => -- ne pijejo vsi
    intro ne_imp -- g pije, a ne pijejo vsi
    have ne_pijejo_vsi : ¬∀ (x : G), P x := by
      intro vsi_pijejo
      apply ne_imp
      simp [vsi_pijejo]
    have nekdo_ne_pije : ∃ q : G, ¬ P q := by
      rw [← Classical.not_forall]
      exact ne_pijejo_vsi
    have ⟨p, p_ne_pije⟩ := nekdo_ne_pije -- p je tak gost, ki ne pije
    exists p
    simp [p_ne_pije]

/------------------------------------------------------------------------------
 ## Dvojiška drevesa

 Podan naj bo tip dvojiških dreves skupaj s funkcijama za zrcaljenje in izračun
 višine ter dvema funkcijama, ki obe od leve proti desni naštejeta elemente
 drevesa. Pri tem prva deluje naivno in ima časovno zahtevnost O(n log n), druga
 pa je malo bolj zapletena in deluje v času O(n). Dokažite spodnje enakosti, pri
 čemer lahko do pomožne funkcije `aux` dostopate kot `elementi'.aux`
-------------------------------------------------------------------------------/

inductive Drevo : Type → Type where
  | prazno : {A : Type} → Drevo A
  | sestavljeno : {A : Type} → Drevo A → A → Drevo A → Drevo A

def zrcali : {A : Type} → Drevo A → Drevo A :=
  fun t => match t with
  | .prazno => .prazno
  | .sestavljeno l x d => .sestavljeno (zrcali d) x (zrcali l)

def visina : {A : Type} → Drevo A → Nat :=
  fun t => match t with
  | .prazno => 0
  | .sestavljeno l _ d => 1 + max (visina l) (visina d)

def elementi : {A : Type} → Drevo A → List A :=
  fun t => match t with
  | .prazno => []
  | .sestavljeno l x d => elementi l ++ x :: elementi d

def elementi' : {A : Type} → Drevo A → List A :=
  let rec aux : {A : Type} → Drevo A → List A → List A :=
    fun t acc => match t with
    | .prazno => acc
    | .sestavljeno l x d => aux l (x :: aux d acc)
  fun t => aux t []

theorem zrcali_zrcali : {A : Type} → (t : Drevo A) → zrcali (zrcali t) = t := by
  intro A t
  induction t with
  | prazno => simp [zrcali]
  | sestavljeno l t' d ihl ihd => simp [zrcali, ihl, ihd]

theorem visina_zrcali : {A : Type} → (t : Drevo A) → visina (zrcali t) = visina t := by
  intro A t
  induction t with
  | prazno => simp [zrcali]
  | sestavljeno l t d ihl ihd =>
    simp [zrcali, visina, Nat.max_comm, ihl, ihd]


theorem lema : {A : Type} → (t : Drevo A) → ∀ xs : List A, elementi'.aux t xs = elementi' t ++ xs := by
  intro A t
  induction t with
  | prazno => simp [elementi', elementi'.aux]
  | sestavljeno d x l ihd ihl =>
    simp [elementi', elementi'.aux]
    simp [ihl, ihd]

theorem elementi_elementi' : {A : Type} → (t : Drevo A) → elementi t = elementi' t := by
  intro A t
  induction t with
  | prazno => simp [elementi, elementi', elementi'.aux]
  | sestavljeno l x d ihl ihd =>
    simp [elementi, elementi', elementi'.aux]
    rw [← elementi', ← ihd]
    rw [lema, ← ihl]