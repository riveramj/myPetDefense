package com.mypetdefense.util

import com.mypetdefense.model._
import net.liftweb._
  import common._
  import util._
import net.liftweb.mapper.By
import me.frmr.stripe.{Coupon => StripeCoupon, _}
import dispatch._, Defaults._

object DataLoader extends Loggable {
  def loadProducts = {
    if (Product.find(By(Product.name, "Frontline Plus for Dogs")).isEmpty) {
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallZo,
        sizeName = "Small"
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo,
        sizeName = "Medium"
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo,
        sizeName = "Large"
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo,
        sizeName = "X-Large"
      )
    }

    if (Product.find(By(Product.name, "Frontline Plus for Cats")).isEmpty) {
      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small"
      )

      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium"
      )

      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Lage"
      )
    }

    if (Product.findAll().isEmpty) {
      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium"
      )

      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large"
      )

      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallAdv,
        sizeName = "Small"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumAdv,
        sizeName = "Medium"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeAdv,
        sizeName = "Large"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeAdv,
        sizeName = "X-Large"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallZo,
        sizeName = "Small"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo,
        sizeName = "Medium"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo,
        sizeName = "Large"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo,
        sizeName = "X-Large"
      )

      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallShld,
        sizeName = "Small"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumShld,
        sizeName = "Medium"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeShld,
        sizeName = "Large"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeShld,
        sizeName = "X-Large"
      )
    }
  }

  def loadAdmin = {
    if (User.findAll(By(User.userType, UserType.Admin)).isEmpty) {
      User.createNewUser(
        "John",
        "smith",
        "",
        "rivera.mj@gmail.com",
        "password",
        "(404) 409-0724",
        None,
        None,
        None,
        UserType.Admin
      )
    }
  }

  def loadGroupons = {
    if (Groupon.findAll().isEmpty) {
      val threeMonths = List(
        "GCSSRDDD","RSPQGCSS","EYDCSZXK","BKGNFSDJ","BKYRYQBT","YFTDPCHR","TNDUNSRA","ZPKUVEKS","VWYCJDMC","NFFYHCFR","JQZTUKVS","ZAPYBNUG","WMDFRKVD","YTZYKAMJ","ZRYHAZAN","YUKYMRHR","PBCZEMRU","ZZTEKWAF","VQADPWVR","WMQXTJKY","CXRHCKNA","MBJCVGND","KPDVCJVA","ASQEZCQE","PBPJRPNN","NGKSHSBV","FVDDGHJG","QSUFPZUB","KMCYBXFK","VSSDTVRJ","EAEMZEMM","JKVSBUTQ","SNZHYABT","RDSDXSBC","ZVTKJAKQ","MYMRQMCT","DBSUSDFP","RPSVBZTW","VRPJKTWD","CDVXYSJJ","PXTJYNXH","SQQZEQKF","RCPGVYPH","NRGZHXDA","ZHBPMYGB","VSACXKNG","EUFVCVKA","CWYWDCQQ","XYYYYXQT","MNRTQWRJ","MYMKFBQX","MDXEDSWR","KNTCUUKD","QGEYSXEJ","WYFCNTDP","RBBEUNUS","STWVRJZS","FDDDCVCK","STCDCCAV","UMQZYQTV","QNAXJQER","AFQASUFR","GXBJWMKH","FMZCNJNP","HXHPKFBM","XJSPDSMD","GFSNUGPA","CMKXJQXM","YHDDTFCA","KEAMPXSG","JFZPGYTU","ZHRWSARF","PYTQXBVY","TNGEEXAW","WGVZMDFY","TCMWUTXG","QJSKZCPW","RXBUZJEC","CXAEFPVC","TPBEKQSR","BPEEQXQG","PGMXKKNT","HCASUTPM","TQSYTNNK","VNFZNFSJ","RRJDPTCH","SYXSTZRZ","VRHWNSYK","YNUFTJMV","YEAYAUTH","SZRJZQKA","UYFVUNTD","QXSZBFEV","RKGXPHVH","BERVZKUH","JGPVFWXG","XUEHUBWK","TDYJVQWC","MHSSSHFH","DXRBBEWP","QEXJRNJB","CNYJVZYN","WCSWAJCH","BDRQHPNR","PCBABQNY","HYHNHUVH","UQZAXGMJ","RENAGJTK","ZMPPJKMR","GGWJTSSW","HRYFRQXY","RSFVRVHY","GHEGEBAD","MACDXRXD","FBTZPNTV","RASAHARQ","VMVFPWTC","QDNKKRTG","DNCSDCPK","ZDZENJED","KDVSVBRF","EXKQQWMZ","NZEPFGCT","REDXPJVH","PHPWAKGR","QBVPQSZX","WBWANFYH","DYTVDUVJ","PHFRAYTP","ZXFGDUUU","QJEUHPEF","CCMRKUED","SCCYYAMM","CECMWKAF","KZZTVNPA","SFJJDWFC","PHWBKPAC","YGUUEBRG","NPMPFCRQ","GFCEQWTB","PRUTGETJ","TNQNRBXC","XZWTMJMG","YVPGHTXG","JGBDFEWD","HHDXGANF","FHPSTMXH","XESNTYKN","XVWXTFMG","PCVYJMBS","XRMAPFHJ","ZKFPNTCF","MBGJRZNG","HJHRDWKM","TCCYMJYK","YVZPZBKM","WAYBFWBG","XAQRCDPV","FKGFATYE","RSMCHRUZ","WCCMXMKZ","UEDNSUEQ","PXXSVBZH","CXWNPZAW","MXXSUQBS","BXYZJAYY","RJNXMTVD","KREPZZAT","GEUGHPNG","FUYVXSQY","CZDWECJZ","AFWPVXGH","CFBPFDXD","NYZUETAG","HFZVDHRF","EPVKCMNP","WZHDZXHE","JGVBTMGC","FNKEGRTY","BAQHBQBT","TJAUPSKK","VWGUBUVY","HHGVXKCB","MWYYWEGH","ZJWMRRBZ","HATFVJDR","AQUXQHDK","FGCKYXMP","PFJRCUSX","FSZWSZTX","ZXSUYTMG","URGMXFSE","WNFEAVQZ","AFFHJBCS","CBDSMPET","EDXWPKMA","RCJQGFTQ","ESFDCMZQ","WFUZXBPH","GMHBQPAE","TKWTUBZM","UMWXQATJ","ETVAAQHW","GKBVPCFB","BWJGMQJC","NYPVNJWS","ADZTZNFX","JNSKYMAR","CWSBFCBZ","VNGTBJCT","XRYPCSAZ","DFMUEXXV","EDDNCGAE","EFDFHJSB","VKQTMGUA","JAJSQEXA","ENJGKDEJ","HVAUCNTK","BMCDBMRF","FSJGTWZY","XSFPGZHR","NDFVSMKD","UKPQBDDQ","STJGXVST","YTPNFQQX","WEKXUNNP","AVWWUVKJ","RMTQBEMQ","CVDYRRXN","RSFAFDRH","JETVNHMA","RPGBZUWF","JPEVVPBZ","EQVRPCCS","ZXYDXSZH","NZDEVKXJ","EHJRFKTJ","GBMMKGYN","QNPSSXUP","RASHCJJP","XKRXTJQW","MHRSVXFP","XSHHQMNF","HJZEXZNT","UKSBWJRZ","MZVVJUVM","ZWFZQKJQ","CAYYVRRF","YVMVHVZN","KPNMUGAT","YPRUMCFA","WNSHCPVH","BEZWFXRD","HAJKVVVP","EKSSQRNU","RNBPFHVP","JMFXTNSX","HAGTJZRR","UVPGWXMD","KSQBYWJN","RZDEWGYZ","GXUSTRVV","EUDUZVEV","JBACSHBQ","HESNTKGQ","XAAPHUAG","NSJRMCMJ","XFCBMBGA","MJZSPRWK","EEGHKFRD","QPECDSQY","HCEEGFRM","YRWNJXPN","UXGBHEGU","ZVBBSKYV","RWSJJPXG","EGWVJHMN","HGXHCSQC","XECESETE","DFBVQWKC","ZJHFTCHR","ZBNGUAMZ","BMVVRVCC","BTQQKTPP","CEPVABFS","UQBHDKSX","YTURAPRW","DWXCZMJQ","WJJAVRPW","PUWBVNTZ","QXWFKJRR","NCXGYFZP","MGBMRYPF","SSSGCKZZ","KDPQNQYB","GRPHDXVS","WCNSUEFU","WZQJKJRH","WYZJVPDD","KRQBYDWC","PDYXZVNE","ZAQKYEVA","GPGKRBEW","ZKTAXYRE","NRUWSCQR","KQHNRZMT","THKNNSEN","KCARKEKC","FKEWNCHF","NEKXZMTQ","TYDKKRHP","ZKBREYHK","ZBMZXTPB","RKVEMDVJ","ZPKVPTEN","UYYETKYE","BYSAWMFP","UBRWUXHE","NQMKRJPR","EVPKMAPH","JTSDBZKQ","HUXVBURE","XUJGZHSW","XPGPKUNP","ZVSGYEEA","MQTSZFNJ","GFVRHPAM","AKKKBQYN","RRUZGCTG","KGJAEGBE","VBDRGUWY","VNRMNKDK","NTUTNXSH","KHUEJQWN","WNCHKEEM","WDFKCZPF","MYMHRAHV","DXUJERPD","CKWNXUXG","ATEYUEDE","HTKCDBSG","CHEXDQAK","JQBUKEAE","YDXDFZFF","XTMESARA","KWVQVWDW","PYXKHMSB","YYXDVZRJ","KHPQDDPM","PUXRSAMU","GWVNNWDY","YRMCEGSB","THGYGFWW","PJKMNCSM","BTQPCEME","SYRPPCSG","JXNBKNXY","VAVSXUQM","BFUAVKBH","BARAXKAH","TZBXKBYP","JKDUGUJA","SCEWQERZ","YCKYVGMS","YWXJXFMR","NWWGJQYD","VSVUFYXN","JQDUKGDZ","PQBHAHKN","FDBWKHKS","KXWQKFHZ","ENWJUWMP","RWDXCHNE","TJDFYGXA","XTQHPCAV","UVBKYTDY","PYSYEGAF","NSYBRADB","YQPKQJBY","JRHCSMUU","MXMWYSJB","FZMJSKFJ","ZKYFNDFF","SAZBQZSU","SGKDMSYC","DDZVHCYM","AGXRHYZH","MSUAPEZS","NGAEDFCN","RHSBTEZC","TPAPPDUC","RDUVSCFJ","EHNYZQDB","HAVFKTCM","PNREPZBQ","AGKZHVQY","KANXWPEY","DQVMWUGR","VMZTEWGH","SPCUXTDK","PJBGKGHD","NYECVNWZ","ZBFXYJDQ","FYQKFPKD","QWKBVNKN","UQWCKRBM","GKDBHHPM","BAUPECHX","SNQHAFTS","XTDWFZXR","BWYSRFFS","KDWXFASP","MDARAKQF","YDEGTRCK","AFCVHTBZ","MTPGZRHK","YHFTZMKG","YUGCAVVV","KADMXRTY","KSFGZFNR","HGWMDWUH","PPTSRHGD","XFQVTCCZ","PWJSRVXP","AETKPBWZ","VBFWBKCW","TKTPPVKY","QMBGSXWT","FTNTKVWE","WVZAHMCC","VWRAQCUZ","FYGMCDFU","USBFUYAY","PMAQRKVP","CFZWUVQE","FVKEFNWF","KKHSXQHB","NBVSSYWR","GPUEAJRH","UEGVMGZS","EGVSNWQZ","FUFQPWRS","NVETUXGB","QTKTCYSZ","YKVYMBCV","XTZGGMPR","MEWUWRSF","FBYDVXXM","UYFJNGCG","CFDZEFEX","FCVDNKFM","SYVQWPCR","GZGRKNED","HGJHKTVP","QMAAEJUU","PUHNPAAX","NVVFXYYV","XXFRCUCX","PDCCWSDC","QBBADSDN","FXXEETYD","SFWRUFDM","TBHZZXHH","ZVBBCFQM","NZDEFMJB","VWCXVPJB","WJGPWBMT","PWMGTDAE","SNYYKUQT","KDNSYXQN","NJBVUTQJ","UXPRJJNY","NCCXYRJB","EVYWFEXM","HPYDWUBA","ZGBVTURE","CGNFSXNR","SNWUMBWE","WJKXYDYQ","YEBQWUHP","HFBWAGFP","VUXUDRCJ","HZJPWBQB","PCTKVVMR","GZSFMSAH","MYHZXCTB","GRNTTEFF","KVYPRYHD","KANFHWDE","WZPRJGMV","WVWKKUNM","NTGNUKGD","TKWBXZYS","YTQFRZEB","WGXEQDDK","SGWWUTEU","SDQUFPSP","DGJPZUCH","NYUFSPAZ","VZRGJBTR","BDZXWCUH","QKSRMSXU","FUXUDAZQ","JVFTJFQM","EYDTRMUX","TAVSXNHC","SYHYKYYW","ATPXMRKS","MRTTSPDX","KCGPSRVK","AUMSDEPU","ANFFBCFE","RTGCWZWK","BGZSQWUN","FZBSCBUU","YRJMKSRX","VDEBDFST","PRPGGKKA","JTFSAQTH","AQPEFDPE","UQWZGDZM","ZXGEGYFU","HYNYHXPU","ZCQXHYSV","VKJBYGAP","HWDNYGGQ","VMVEUBCT","HUTGDVQH","MAWSBHQX","CPBVTPAP","DZVRXGQD","XXMEGHUA","UDKUEUXH","ADDUVFAU","XZSMPTVV","RUSSHDFX","PUFUFCNX","VJYDTHZW","DZAQCCDG","UXDMZAEW","BWTRQVHW","RADTSYTQ","YVJWQVPT","WXCBUFDZ","KVWSESUF","ZXKZJKKY","RKEKFCXG","MAUWNKZP","ACASHXYV","MFXQPEHM","AJBQBPQH","CNXAZFAA","ZZDZJUYC","BQPSHAZM","PNYYVTMV","AWSJGNGK","VUGQEBKY","JTVWBRZD","KBZAHXYH","TGMZRHBD","BVPFHEQY","PZACVJTC","AGCWAQTB","WWNJVBCB","PCUXHUUB","ZENUASGJ","TGEXZUMM","VFJEZKFU","QFQYTTRB","AGNNHAED","ETQYCHDM","VPRMJFVG","DKNKBFHP","NDRUDUQT","WGWGTTPH","GJPYAJMC","NFMNCKFN","XUDNJSSD","PKKEWATQ","GQCFBGPF","KSYJPNTN","YGMUTUWD","ADSSQEPY","VKKRDPSK","ZXAYEJZW","JMRKGQCR","QTWSUZMA","PSZJKHSK","VRRNUQAW","QTKTTVFU","GGPRAGUQ","PCPGQKJU","QGDCBRWS","ARQWVRSU","XWNPAMTY","WFGMWVCN","FBEHYRKA","GWQTYQAH","EDPJHATP","FNTVCCAE","SXXPYAGK","GHJZSMZG","TJCYSVQF","UJQSQXUJ","CEBYHKEV","PSGPGKAH","BHWATNFF","QKPYHZED","YYDBBZUR","JVVVGSRX","RHXHFHWT","VFWGWWYJ","JAXYBKJZ","KVVDVEKX","VXWVGXFS","TRBSSPUA","PQYMBKEE","EAQBWZPZ","NUSZRMXN","ECXGCCWN","KHSRUHNK","UGYTVNMP","MFZKNKXQ","GSNCDQRW","UVDEGEWG","FKMABKWG","EEJJGGEK","HAWAZXHJ","KPXXXAAY","ZTXCYASY","BFWNHGTU","XBBQCVKE","DRWDAPJT","RSDMSFNN","HNKCWUKP","AVFSWGCK","GXCRZBHZ","HJGMAYKY","GEYUBSAW","EUZQWZBT","NBTZVFNU","NNJUCRZA","EAJUYSWS","ASVRGCTC","QNPFJNZR","WTVQDMGK","UTWBCYEX","BBFYPSXV","TQDCZSWN","DZHWHKXW","TNEWFRQQ","AFKDNJXP","ZQYEHEAX","XVKVDNBV","YWPYCJBP","JQZSPBRW","JGVTFSJR","HTCKKZRA","MDYDKUAT","RXEGBHAU","RJGCZXPE","XHRCTZTW","VQYNRNFX","CHVZAYXZ","UXZJFDMP","SZXJNHNC","ZBJNKMNP","TKCRBMWE","PYFHMSEY","PBSHWNRT","WMSGNFWD","EDEUZYTG","ATTYQQNB","FKZYNNXB","BCSZKYDC","EAZYAAXM","AQUWYDMD","CFAPYDCV","TKPDUUCJ","FPGTQRPV","BTYNUWBN","ZBJUZUBP","NZAKYARD","UAECBYVW","KVDAYUPF","RDXHVVTY","MYVMYDME","CCSMKVKD","ECYPSTZY","GWGZSNEV","DUBGPMUE","ERPHANVZ","PWPCNYAX","MTXWHNAH","NNCVFQJT","XRNFQXGQ","DYWJXZRS","WFSUGZNB","BZJXTHHY","XYPNVCRJ","QHYAKFRS","KHUKRMCN","QUJFMQDF","SKPZTYGX","KNWXVRHZ","UQJZSVMK","YHPKBXPN","CEGRNAKN","TSVDABQW","UJTTAZFZ","GFHHKCJW","AFCYVXBP","GYZVDVHZ","FDTJWSDH","WUTANECA","SYBRRJXM","JVPFMVXS","KDPFZBRU","WADGGMTW","RXDWURPN","FURNPZPE","HVNDSVAW","HZHAKXHB","YGTHYUTU","UUPWXVCS","UJYFJWHN","CKXPVHJS","JAJZGUYQ","KETQTCRV","BFZQTSMM","AYWFCRJG","EAHKEGSS","AUQFYDXC","EJBFYNEV","HQSXJEYC","NWPVKYQK","VXSBTPWQ","UBAEBZYT","HECGJHEX","ZDNFUUBY","UPXCQAVW","DTQUMKGH","PCNHUJCF","JXFMJXCZ","ADUZVCFP","TNCHKRXD","UUUGMHZC","VVRWUFJU","BYJCZWBK","FCMRPNXQ","UJXTEUZY","XRVFTWNQ","ADSUBJQM","YTQQFGBN","ZXQYPMXB","VSGHDZEJ","EABNSGEY","UXGZKWCQ","UFUDBKJB","ATHTRWZQ","SWUGTDAG","WDSTDECB","VDEBVAZC","YBBNRDSV","PTNUUGMW","HEBFEPTQ","YZZPRSVM","ADCMMFEB","XMRKWZDJ","VMDMUDYB","DRMXZBKC","TQHJBMBW","HATZSBEK","SESHMQCR","GCPFVMMY","FDFYQWUG","PQJPNBMK","UAFZERHW","HWTWWSRC","KGFARFKC","BTVFDNNP","AZJSGWCR","GVMGRJXJ","BEGDTBDS","PYSJPRGQ","SKMGNPHT","GJGYTMZQ","ZCHDWGFV","VTBMENSS","CUEFPSMB","HNGMKPYJ","JZZZFMJM","THEGZGNH","HDMYHKGQ","SZHVNWJK","FKVMWUNE","HWXKGTAW","CFVXRZFW","KBADKTAW","AQMSAUST","CQGJJHEZ","AUHPCDYR","UWGUDQZJ","WQAEJQXN","FNVUETGG","CDZRMSZG","XGUPHPDY","WQUDUVTC","HJTAJYPC","DZTSGXMB","XAYJVPTU","WMANJNPE","MDVEWYFM","PWCXYZPJ","GSNQSHRR","PCJBMMQA","GYZAZGSP","CXPHYKEN","YBFGPEVR","DTMMEUZX","AYGNFZSE","YDGCUGWT","RXEQNFND","PZVERVXQ","DXMSNKSR","TVQAYRJX","YZVKCADY","GQKHESCT","GKXFBFKQ","HQBKWBBK","GTCVEWSW","UNWXDPAW","SRMMYAXA","BVTBVAKQ","YEJPGKUV","CKJKFGVY","UUYPTMEP","VWTTDZEQ","XTRJZRNM","FBFFUMTU","UBZMXYFQ","EJRAWVZK","QMPFQKWE","JFCSVKAQ","PAKCZVRM","CPHZJFKT","UFMBSMJQ","GHRHPAUC","WHXHBEEM","RTREABZF","BPVCRYKQ","JVEDNPPH","RAGVMXNC","AAGRBPDV","QPCRCRCT","SVYXZTZE","SSBDZURG","CQKSRYEZ","FMMUCYSQ","VDVHGVDR","TVKGWVYF","GAVNMVHF","KNPGWUJB","EYVGQWQR","ZZUCCCVW","HHNDGPPA","NKBZGWKQ","DUNJWSKP","MXJGRTAQ","JPPGGURN","JQVFKYAN","ZQBXAXMJ","MBPCTVJG","UWSPNDDA","ADJADCXK","WFSJKMQR","PCGPJJYE","HTNFDNUN","JDHAHXYD","AUMANJGK","XEPUUGYE","CWXCGXEN","JYUENKYW","EGGTXKEU","WXSPBYDK","PPKHUNBA","NTTBBKYH","EAQPXQUE","VKRDUYUV","MSJDPUQS","UPJQJPZK","BHBQSEVY","QERTGQTG","PYHBQHPW","VEKFXEKA","UUBRTJTM","ZKKFHWWA","NFXJYSMZ","XDAPYPSY","VWDKEUBF","PSVDFTTU","UZQWPBMJ","KCHMCTFR","AUGKKBYY","BPPRQBQM","FASRFKNJ","PXAUUPZT","RHCJSWYD","RRVESCRU","EBHFQSTV","RDQKNZFC","DNQQQFKX","DYKVEHHT","FUTBXXPR","XSZANPJU","YFTAKZGQ","CXSMVYZR","MYSYYBVX","ZYMCGBNY","NXGNWZJJ","JCPNWYJR","TVBDTSZX","HCAXMEEW","KEAWEJWU","STRHCQET","CDTERFSG","FMTYZHCT","FDZSZQJR","ZJXWUBER","VWCMDACK","GUDDSAPX","DQSFUZHK","HMWNZUWB","VEJSFNUT","SZCGSUKR","FWCEXWPT","TRCDQTKJ","FJEXUHSN","KTFKETAR","EFBUYGSM","FMTCNZNS","UNYKRHGR","KSGAVSZU","TPPHXVGE","JRDFXCVA","CWEQTFPG","APGJVQTV","KTDESSGP","UQGCJRAC","AZZDCDRS","EHXCZNQF","RFAUTTYN","AWXYKJJK","REPAKJUD","HFEJKBSZ","YKGGMASJ","UQQRNJEJ","EXFXDDSR","JATBWXCM","RUJDXGRQ","MGPVBRYN","EYZVJHKD","QWJBJTKW","UAWMDCST","KDDUYVVK","WDFWXATB","QZSBXZQY","BZWZHPYH","SPYKNXCC","PXRKGWHN","UNAUEAYJ","ZKCYVEJB","GWXGSBNE","NMUMXUMH","VJCTXNFQ","YFHATCUT","DAQRUYPK","AJSUKDUQ","PRSJCXYX","ZKCKQKGD","FYRGEBAW","WASCWKHJ","CRBYTPAW","EEJWCMFZ","RARNJGPU","VDQBYCFS","ZGSTTZWQ","MMGRGTST","XNKPDWCE","XDSQMGWU","FCACFMGQ","CTGFDJTH","TNJHXPWH","PZUTKRBK","VYMCTMSR","WQUVSFQZ","YFKVFVTP","WYWNZYZF","DFBVRDZF","MAMWQURA","QYRQGABG","RRBWZTYU","FJRESEGQ","QXEMWSDX","SDJNYACA","FYCAZBNT","RQZBBVMT","UUEXGTTJ","HCYTVUNV","VUEGRHEG","GHZQRFTQ","XJDTVBXW","WEYBRYBB","WEPRYEAY","MSPEDKXT","JRPQPZFV","AKWKBKAR","WSSRBFQT","QYYVAQQK","YCCHRJFV","JJAVNPHN","TXYBUJCC","EQGJFBBX","NQPAKMSZ","TAEZZGSR","CBCWYBAS","YKZXZBEH","DDUMYSXP","GANMNXUJ","QAXSTRUB","AGUWTGKY","QGDVNBRN","EGACDADK","WQJYRQVS","PVWYGPPT","THKBVSYS","YZQNHZHV","XAMFUMVF","DNNPSBGT","KZHZUTJH","MKAHYPDW","GUJGGWYW","TSTCFACK","KSBGSNPZ","PMZVZVDK","ZDVATUGP","PAQVAKJN","ENBTFBED","BDXVQZMM","ZHSQBRCP","GJSPGXYK","WHCHBQQN","AYNEQBYS","FYMNVUUD","ZAWQTGWS","CSFMDTQV","HJUXDGBR","APYPMJTD","HVXYMYZE","VQCBHDDN","AFTQWQHJ","VQTMFASR","PNAFSNZX","BUNXNTHX","QYREDPPG","PVWVGRJJ","YXGNYXJJ","MSVTZUHB","DBEEUGWV","DHKNRYKC","RMHJUTDG","XXKNJBJJ","KRGUBDDT","SPJJEVBH","QSRNUHTE","CGCQVJNB","VKFCAWVB","ETQYFXYU","BMJKAKSJ","KJCEJVKP","YMRXJKXK","XVSDRYYJ","DNZDUZPC","TQRRGRFG","FJBZNXQA","UAZWGDBM","FJPBDFSY","BCTSABVV","TGKUHTMH","YYBJSTTM","ZTQZQWQP","MYDNMJEU","SHHZBGHK","WARCWNVB","DSBFQADD","PDVCVASB","RNHGVBJF","ZBABNZDA","PREYMWYZ","FYAEBUES","QBPHGPCR","EZMAWPDH","VGPHWQHB","WJUHVTVC","NMSUVRQJ","CYNRFARW","WHQRXXXS","ZSDENZNJ","MDRYJZRF","YEGJJYYG","YEWUDPSH","BYEVQPBP","PDQWJUEK","FGGPKXMS","QZQNXYPS","HXFHAVAX","TRKEJWRW","YVHZARMZ","DACMDUGX","QVWRWVXT","PNZENTHX","ZTSXQDQK","GRRBMCTT","FBUDAKRK","JHKVTWBX","KGTJBKBT","FAVMNTQX","GYXQCWUK","YJSZKRBS","GGDZSCDZ","JDNJMSCS","AGXUQNEM","FBSMYEVJ","RBNMQDTM","HSGDZZZQ","GFGNANNP","YHGDJENN","KPAMTFZG","QUCXXXBA","YMVTHZNX","NCFBSYTU","UDARWYQV","UKMNYWJM","JCUUHDQW","UMDUGKTX","JJEDFGAB","XCXTJCYQ","FQZCMFRJ","YSMQCWHS","SUGCKGVM","RSCSJTSB","QGFBWMBQ","BFJHPQKY","YBCBGKWJ","HDAXNDHB","WGXTRHAJ","ZVFJYXJJ","ETVHJCZG","CBGQAGTM","AWTMMWQB","JZXUQSUW","ZHHQVWKS","NANFVZFC","URARAADD","PZXKNYXR","NGCWXXJP","EMFJJFVD","VWDDKHDY","KHCHYUYE","ZGNZTTQS","BJJUKNEC","BRWUUBYF","WUYGXUHP","XXZKCZCW","VHPGWCER","JTMYDZAY","WMSPRVPZ","EBQNHACM","CATNGPAX","FZRMHDQB","ZJEBQYJM","SEZFURRV","VFDFPQNK","JHAKGRBV","REDDPFMV","KBXMVBFA","GHBEACYU","ZNHEZUTP","CTKWUYDE","HRRKPJQC","WQKRUPZR","GZCZDFSX","XZDJTQJC","ZEZHCDGY","ZJRBSGRA","YYPMMPZY","RXGAYAVA","VWXEQMNJ","GTEMSBSZ","WBXEXKVR","BFRXGPKG","YSDCXXYW","WSYKRJWS","KTBABNGP","MGUPNEUZ","FNHRSQJQ","SVCQACPQ","RVVEYAFK","TTQYJMGK","GQSWFUTH","MXYCQVVG","HBFJYUHA","JUXYFEHU","PXVRQEAT","JABQEABV","NSACTXXX","TKQCRCGV","EVHYSEHQ","RKVDZBDM","VFHRZYHU","GQBBVMCQ","QKGMCAYD","PBEXMDYB","GWCBGEGE","GBSHSTUQ","GDBREFSS","YJPCCAWK","WKKBYQMF","MJKDEQWG","CTMZGRER","YNKJTPBA","JVWNTJFN","AVHXMHSJ","NWZXYQEV","JSNRFHNM","NUUYAMRD","UDSKKVDF","XDTZWBDP","KCDNGMZT","KWEXCXVN","HGTQMPPN","UMNWTJWK","NSEBQJBA","AHAJFWRJ","PFDYSFDN","VHYDPZYK","UZQCQFMA","USNVVJCM","DYYQHCQP","ACYUDETE","JRAKPBNH","PMVRURQN","WVJVGDVV","QNGJRJJB","XVPJRAWW","VFWETMKQ","YANHSNWG","AZWVEZMR","NNGWTJAD","NVVZDKGD","JKJGPFGB","JEVTMMXY","JWGYXEAR","KXNZYAEB","CRMYVHWP","SBNGYVEF","GQXWTUBZ","BZZXRMGX","YWNWPNDX","EGNFJWBV","KJJYXFJT","ZYVMXWEG","YCHWJHYT","MYTSNNGS","NJFGSEYU","MDJCKVJP","RKDVPMGC","VSQYKSWV","JQWHEGHT","HDEMUUCR","MVDXHCJB","DNCHCERM","UGMFVSZP","YDZNAEUZ","QNGXXRCE","ABVVSAHD","VHFXSPSB","NQAQNQMM","VGQYYUYM","XTAUXPPM","DVZXANQN","DZRFEJKZ","PVYXUYQU","ETKQDCRS","KHZRUPBA","PZXCNWBY","VYCERBQQ","CWTWMFQT","XEMMJFKV","DSNZEASP","UXQCXDDB","QGFFAHCE","ZNAKSEMS","SMTEDDCE","RZXEXPER","FHFYHFTJ","PFYACHUK","XMGVSEAK","FTYYSKNM","FEFJKPKT","KFJJUARV","WSCGRGNZ","MAGMUZJS","XAXVAVTR","ZZYHYGGR","FPTWRBNT","NPDWJDEM","ATMWWEBZ","KEFBERUG","AWEZTHND","SQRFWUWH","EUXHBYZJ","DVSVXPUZ","MCZTJCRH","UBZFHBYT","XZYTWNEA","AZXVBFNN","RGHQPYXR","NGMCWUED","MSFVTJAH","KYHCEBRT","BPNZJVPQ","XBENVHVP","JMPZVYTV","ZYCAPHJR","ZAQVZFEB","RBJTXUMK","QTUPZJJU","BQRTFFEX","DUMRUHEX","KNBAKHYA","EUDPKDQC","HXCRMAAZ","TFDPQAAB","XNDQETQU","AEAUBXAB","KWZHDFBN","FGFNMYCU","AVDPDDJB","EWCAGHJM","DWGYZMKQ","VADQVRSB","UTJAQHPG","FAJUZYBW","XXZJGDGB","TEERJCVM","RKJCSNCQ","AGBYABPJ","TBXAMQZP","NPTAPGMU","GQXDCTJP","SQMYZFWJ","YGPECEFG","UCXYUZVJ","MBVMKBMB","WGWHCZQZ","UMSQSPED","FSAENDVF","ESYGDWWG","XGUABFQA","SYQWCEEA","XDBADTJT","TMVFZAUP","JNMHRWYG","SPXKJCUB","XSXCPKNR","QVDKXZYR","DGDDDYMN","MRRGDKSA","VCCTKMKT","CDMYPBQC","PQRRCFJM","RSTUTDNZ","HMVQPCYX","EZWEBJMQ","CTXUJHEB","QBMVZJSZ","TDTWMXYE","SXMTVCRZ","WWKFPQHP","VANDCJFC","UMCZGPMW","REWMWQYQ","RQNYTVAQ","WCCUYEXG","UENZBXXP","KEQRMZVH","MCYCUQFE","JQBXNRWD","XAZZETQS","SERQDNZW","RPSVMQVR","PQPBNZJB","ZYMQXJZR","NPWADZCR","FWPDGCSV","SCDUFPBB","NXYTBYKY","XUJFHUUY","KFKYGNPR","WQRTMTNH","APKDHDYJ","VGSRVSFF","DZRCZGVN","DAZUHVTJ","PNJYJBYG","PQNQGFKW","JTUKNEVA","UPJWFFKQ","MVTWZXVS","QJMRJSFJ","JEEUDSBR","XMTZRJJC","WZTBGEDS","CTVZPRKG","KKBMJAUN","DGQZXHED","BUWGBUJN","RVXYMTMC","YHCGXTDP","FKJDQBVN","DQRBTBBC","HWCVJQXV","TVEYCFBZ","BUUBYZRE","FZDEVFAA","NDFHVBDE","PBUFTCYZ","NDGWBPBP","PWFSFDTQ","RTDTEHKS","AZUYZJNG","ZNCYJCSV","VTCATGFM","NRNBVZKN","JXMXFBHV","KQUPJASC","VJNJPAMG","QACHQBBW","PAMNXVRG","TSPSPHZU","RCAPNMKG","MGHXQQXN","URGHQAED","NEJNRAKQ","KKCSMYTS","ZGEPTUGG","YHATTQDN","CXGTZDHH","TNNWYTZS","SZZDQEHM","SVRPDHRC","YJTXDRNZ","JKHUPJKE","ZXTWWGKZ","FYQWSHQT","ATPKENVG","XNSKGMMQ","GQFEPAEC","RXKGTSXE","PUUXHNXB","TDSVDXKJ","HMAFVMMF","ASKXHWCA","MZMPZBMD","UNYBBQGX","AEDJZCXQ","HRXWGSAG","XTESEMKX","VFZFEXPD","EMKPJGPR","RJKPJCZA","JGDQEQHE","ZJHPDKJQ","FUEZCRUA","CQTZZXKE","CTNSDEHR","DUCAGZVZ","FSUUWEQT","SEXDTBFM","TZRGKMTG","BUBAXXXT","XNDAWJJN","BUQNMKNP","JKFHXNGK","VVRDQQJB","CFFNAQFS","RENRCGBT","NVJPWSVZ","THUBEHYE"
      )
      threeMonths.map { grouponCode =>
        Groupon.createNewGroupon(grouponCode, 3)
      }
    }
  }
}
