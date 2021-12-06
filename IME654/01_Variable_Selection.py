import numpy as np
import pandas as pd
import random
import statsmodels.formula.api as sf
import os

os.chdir('/users/juwon/OneDrive/GradSchool/RStudio/IME654')

data = pd.read_csv('kc_house_data.csv', sep=',')

def variable_selection(data, target, direction):
    var = set(data.columns)
    var.remove(target)
    current_score, best_new_score = 0, 0
    if direction == 'forward':
        inmodel = []
        outmodel = var
        while outmodel and current_score == best_new_score:
            score_candidate = []
            for candidate in outmodel:
                formula = "{}~{}+1".format(target, '+'.join(inmodel+[candidate]))
                score = sf.ols(formula, data).fit().rsquared_adj
                score_candidate.append((score, candidate))
            score_candidate.sort()
            best_new_score, best_candidate = score_candidate.pop()
            if current_score < best_new_score:
                outmodel.remove(best_candidate)
                inmodel.append(best_candidate)
                current_score = best_new_score
        formula = '{}~{}+1'.format(target, '+'.join(inmodel))
        model = sf.ols(formula, data).fit()
        return model
    elif direction == 'backward':
        inmodel = var
        outmodel = []
        while inmodel and current_score == best_new_score:
            score_candidate = []
            for candidate in inmodel:
                formula = '{}~{}+1'.format(target, "+".join(inmodel-set(candidate)))
                score = sf.ols(formula, data).fit().rsquared_adj
                score_candidate.append((score, candidate))
            score_candidate.sort()
            best_new_score, best_candidate = score_candidate.pop()
            if current_score < best_new_score:
                outmodel.append(best_candidate)
                inmodel.remove(best_candidate)
                current_score = best_new_score
        formula = '{}~{}+1'.format(target, '+'.join(inmodel))
        model = sf.ols(formula, data).fit()
        return model
    elif direction == "stepwise":
        inmodel  =[]
        outmodel = var
        outmodel = list(outmodel)
        count = 0
        stop = 0
        while outmodel and stop<2:
            score_candidate = []
            count=count+1
            if count%2 ==1 or count==2:
                for candidate in outmodel:
                    formula = "{}~{}+1".format(target,'+'.join(inmodel+[candidate]))
                    score = sf.ols(formula,data).fit().rsquared_adj
                    score_candidate.append((score,candidate))
              
                score_candidate.sort()
                best_new_score, best_candidate = score_candidate.pop()
                if current_score<best_new_score:
                    outmodel.remove(best_candidate)
                    inmodel.append(best_candidate)
                    current_score = best_new_score
                    stop=0
                else:
                    stop=stop+1
            else:
                for candidate in inmodel:
                    formula = "{}~{}+1".format(target,'+'.join(set(inmodel)-set(candidate)))
                    score = sf.ols(formula,data).fit().rsquared_adj
                    score_candidate.append((score,candidate))
              
                score_candidate.sort()
                best_new_score, best_candidate = score_candidate.pop()
                if current_score<best_new_score:
                    outmodel.append(best_candidate)
                    inmodel.remove(best_candidate)
                    current_score = best_new_score
                    stop=0
                else:
                    stop=stop+1
        formula = "{}~{}+1".format(target,'+'.join(inmodel))
        model = sf.ols(formula, data).fit()
        print(count)
        return model

def cro_mut(mother, father,crossover,mutation,length):    
    index=[0]
    index=index+random.sample(range(1,length),crossover)
    index=index+[length]
    child1=mother
    child2=father
    for i in range(crossover+1):
        if random.sample(range(2),1)[0]:
            index2=index[i]
            index3=index[i+1]
            child1[index2:index3]=father[index2:index3]
            child2[index2:index3]=mother[index2:index3]
    for i in range(length):
        temp=random.sample(range(mutation),1)[0]
        if temp==0:
            if child1[i]==0:
                child1[i]=1
            else:
                child1[i]=0
    for i in range(length):
        temp=random.sample(range(mutation),1)[0]
        if temp==0:
            if child2[i]==0:
                child1[i]=1
            else:
                child2[i]=0
    return [child1,child2]

def GA (data, target, iteration, population, crossover, mutation):
    for i in range(iteration):
        # 첫 세대 구성
        if i==0:
            var = set(data.columns)
            var.remove(target)
            var=list(var)
            var_length = len(set(data.columns))-1
            one  = np.ones((population,round(var_length/2)))
            zero = np.zeros((population,var_length-round(var_length/2)))
            old = []
            temp = np.hstack([one,zero])
            temp = temp[0]
            for ind in range(population):
                random.shuffle(temp)
                old.append(list(temp))
        # 염색체별 모델 구성 및 평가
        score = []
        for ind in range(population):
            temp=old[ind]
            inmodel=[]
            for ind2 in range(len(temp)):
                if temp[ind2]:
                    inmodel.append(var[ind2])
            formula = "{}~{}+1".format(target,'+'.join(inmodel))
            score.append((sf.ols(formula,data).fit().rsquared_adj,temp))
        score.sort()
        next_population=[]
        # best 염색체 2개 보존
        next_population.append(score[-1][1])
        next_population.append(score[-2][1])
        # Deterministic approch
        parent_set = score[round(0.7*population):]
        # 다음 세대 구성
        while len(next_population)<population:
            m=random.randrange(0,len(parent_set))
            f=random.randrange(0,len(parent_set))
            mother=parent_set[m][1]
            father=parent_set[f][1]
            next_population=next_population+cro_mut(mother,father,crossover,mutation,var_length)
        old = next_population
    # 반복이 종료된 후 마지막 세대의 best 염색체 선정 및 모델 반환
    for ind in range(population):
        temp=old[ind]
        inmodel=[]
        for ind2 in range(len(temp)):
            if temp[ind2]:
                inmodel.append(var[ind2])
        formula = "{}~{}+1".format(target,'+'.join(inmodel))
        score.append((sf.ols(formula,data).fit().rsquared_adj,temp))
    score.sort()
    temp=score[-1][1]
    for ind2 in range(len(temp)):
        if temp[ind2]:
            inmodel.append(var[ind2])
    formula = "{}~{}+1".format(target,'+'.join(inmodel))
    model=sf.ols(formula,data).fit()
    return model

model1 = variable_selection(data, 'price', 'stepwise')
print(model1.summary())
model2 = GA(data, 'price', 10, 30, 2, 100)
print(model2.summary())
