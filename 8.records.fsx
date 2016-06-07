module Records

type Item = {name:string; weight:float; uprice:float};;

let item1 = {name="mele"; weight=3.4; uprice=2.5};;
let item2 = {name="pere"; weight=1.2; uprice=3.2};;

let computePrice(item:Item) = item.weight * item.uprice;;
