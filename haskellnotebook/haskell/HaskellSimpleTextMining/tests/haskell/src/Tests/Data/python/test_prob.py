""" 
 Bayes classifcation based on Toby Segaran's PCIL code.
"""
import re
import math

def getwords(doc):
  splitter=re.compile('\\W*')
  print doc
  # Split the words by non-alpha characters
  words=[s.lower() for s in splitter.split(doc) 
          if len(s)>=2 and len(s)<20]  
  # Return the unique set of words only
  return dict([(w,1) for w in words])

class classifier:
  
  def __init__(self,getfeatures,filename=None):
    # Counts of feature/category combinations
    self.fc={}
    # Counts of documents in each category
    self.cc={}
    self.getfeatures=getfeatures

  def incf(self, f, cat):
      # update fc set count=%d where feature='%s' and category='%s'" 
      self.fc.setdefault(f, {})
      self.fc[f].setdefault(cat, 0)
      self.fc[f][cat] += 1
  
  def fcount(self, f, cat):
    # 'select count from fc where feature="%s" and category="%s"'
    if f in self.fc and cat in self.fc[f]:
      return float(self.fc[f][cat])
    return 0.0
    
  def incc(self, cat):
    # "update cc set count=%d where category='%s'" 
    self.cc.setdefault(cat, 0)
    self.cc[cat] += 1

  def catcount(self, cat):
    # res=self.con.execute('select count from cc where category="%s"'
    if cat in self.cc:
      return float(self.cc[cat])
    return 0
  
  def categories(self):
    # cur=self.con.execute('select category from cc');
    return self.cc.keys()
  
  def totalcount(self):
    # res=self.con.execute('select sum(count) from cc').fetchone();
    return sum(self.cc.values())

  def train(self,item,cat):
    features=self.getfeatures(item)
    # Increment the count for every feature with this category
    for f in features:
      print "----> train=%s" % f
      self.incf(f,cat)
      # Increment the count for this category
      self.incc(cat)

  def fprob(self,f,cat):
    if self.catcount(cat)==0: return 0
    print "-->fprob %s" % self.catcount(cat)
    # The total number of times this feature appeared in this 
    # category divided by the total number of items in this category
    return self.fcount(f, cat)/self.catcount(cat)

  def weightedprob(self,f,cat,prf,weight=1.0,ap=0.5):
    # Calculate current probability
    basicprob=prf(f,cat)

    # Count the number of times this feature has appeared in
    # all categories
    totals=sum([self.fcount(f,c) for c in self.categories()])

    # Calculate the weighted average
    bp=((weight*ap)+(totals*basicprob))/(weight+totals)
    return bp

class naivebayes(classifier):
  
  def __init__(self,getfeatures):
    classifier.__init__(self,getfeatures)
    self.thresholds={}
  
  def docprob(self, item, cat):
    features = self.getfeatures(item)
    print "docprob: len features=%s" % len(features)
    # Multiply the probabilities of all the features together
    p=1
    for f in features:
      p*=self.weightedprob(f,cat,self.fprob)
    return p

  def prob(self,item,cat):
    print "prob: total count=%s" % self.totalcount()
    catprob=self.catcount(cat)/self.totalcount()
    print "prob: catct=%s" % self.catcount(cat)
    docprob=self.docprob(item,cat)
    print "prob: docprob=%s" % docprob
    print "Cat Prob: %s" % catprob
    print "Doc Prob: %s" % docprob
    return docprob*catprob
  
  def setthreshold(self,cat,t):
    self.thresholds[cat]=t
    
  def getthreshold(self,cat):
    if cat not in self.thresholds: return 1.0
    return self.thresholds[cat]
  
  def classify(self,item,default=None):
    probs={}
    # Find the category with the highest probability
    max=0.0
    for cat in self.categories():
      probs[cat]=self.prob(item,cat)
      if probs[cat]>max: 
        max=probs[cat]
        best=cat

    # Make sure the probability exceeds threshold*next best
    for cat in probs:
      if cat==best: continue
      if probs[cat]*self.getthreshold(best)>probs[best]: return default
    return best

class fisherclassifier(classifier):
  def __init__(self,getfeatures):
    classifier.__init__(self,getfeatures)
    self.minimums={}

  def cprob(self,f,cat):
    # The frequency of this feature in this category    
    clf=self.fprob(f,cat)
    if clf==0: return 0

    # The frequency of this feature in all the categories
    freqsum=sum([self.fprob(f,c) for c in self.categories()])

    # The probability is the frequency in this category divided by
    # the overall frequency
    p=clf/(freqsum)
    
    return p
  def fisherprob(self,item,cat):
    # Multiply all the probabilities together
    p=1
    features=self.getfeatures(item)
    for f in features:
      p*=(self.weightedprob(f,cat,self.cprob))

    # Take the natural log and multiply by -2
    fscore=-2*math.log(p)

    # Use the inverse chi2 function to get a probability
    return self.invchi2(fscore,len(features)*2)

  def invchi2(self,chi, df):
    m = chi / 2.0
    sum = term = math.exp(-m)
    print df//2
    for i in range(1, (df//2)):
      term *= (m / i)
      sum += term
      print "-->%s / %s %s" % (i, term, sum)
    print sum
    return min(sum, 1.0)

  def setminimum(self,cat,min):
    self.minimums[cat]=min
  
  def getminimum(self,cat):
    if cat not in self.minimums: return 0
    return self.minimums[cat]
  def classify(self,item,default=None):
    # Loop through looking for the best result
    best=default
    max=0.0
    for c in self.categories():
      p=self.fisherprob(item,c)
      # Make sure it exceeds its minimum
      if p>self.getminimum(c) and p>max:
        best=c
        max=p
    return best

def sampletrain(cl):
  cl.train('Nobody owns the water.','good')
  cl.train('the quick rabbit jumps fences','good')
  cl.train('buy pharmaceuticals now','bad')
  cl.train('make quick money at the online casino','bad')
  cl.train('the quick brown fox jumps','good')

#*************************************************
"""
 Main Entry Point
"""
#*************************************************

if __name__ == '__main__':
  print "Probability Tests"
  cl =  fisherclassifier(getwords)
  #cl.train('my dog likes chicken yes', 'good')
  #cl.train('viagra', 'bad')
  sampletrain(cl)
  print "Cat Count=%s" % cl.catcount('good')
  print "prob1=%s" % cl.fprob('dog', 'good')
  print "prob2=%s" % cl.cprob('dog', 'good')
  print "prob3=%s" % cl.weightedprob('dog', 'good',cl.cprob, weight=0.3)
  print "prob4=%s" % cl.fisherprob('sss bbb ddd', 'good')
  
  # --------------------------------
  cl = naivebayes(getwords)
  sampletrain(cl)
  print "Cat Count=%s" % cl.catcount('good')
  print "prob1b=%s" % cl.fprob('dog', 'good')
  print "prob4b=%s" % cl.prob('fox dog', 'good')

  # Test feature count bug
  print "---- Feature count bug"
  cl = naivebayes(getwords)
  cl.train('the the the the the', 'good')
  print cl.fc
  print "Done"
