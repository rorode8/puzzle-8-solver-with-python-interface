import pygame, sys, os, random
import subprocess

def divide_chunks(l, n): 
      
    # looping till length l 
    for i in range(0, len(l), n):  
        yield l[i:i + n] 


class SlidePuzzle:
    def __init__(self, gs, ts, ms, begin_state): #grid size (tuple) , tile size , size of margin
        self.reading = False
        self.gs=gs
        self.ts=ts
        self.ms=ms        
        self.tiles_len = gs[0]*gs[1]-1        
        self.tiles = [(x,y) for y in range(gs[1]) for x in range(gs[0])]        
        soundObj = pygame.mixer.Sound('slide.wav')
        soundObj.set_volume(0.3)
        self.sound = soundObj
        self.unleash = False
        
        
        self.tilepos = [(x*(ts+ms)+ms,y*(ts+ms)+ms) for y in range(gs[1]) for x in range(gs[0])]  #actual  pos on screen
        
        self.tilePOS = {(x,y) : (x*(ts+ms)+ms,y*(ts+ms)+ms) for y in range(gs[1]) for x in range(gs[0])} #the place they slide
        
        self.prev = None
        self.speed = 300
        
        self.rect = pygame.Rect(0,0,gs[0]*(ts+ms)+ms,gs[1]*(ts+ms)+ms)
        
        pic = pygame.transform.smoothscale(pygame.image.load('husky.jpg'), self.rect.size)
        
        n=gs[0]
        nums = list(range(1,self.tiles_len+1,)) + [0]
        
        self.matrix = list(divide_chunks(begin_state,n))
        for i in range(len(self.matrix)):
            for j in range(len(self.matrix)):
                if self.matrix[i][j]=='b':
                    self.matrix[i][j]=0
                else:
                    self.matrix[i][j]+=1
                        
                
                
        self.images =[];self.font = pygame.font.Font(None,100)
        
        #for i in range(self.tiles_len).reverse():
        c = 0
        cx = cy =2
        tups= begin_state
        n=len(tups)
        if(tups[-1]=='b'):
            n-=1
        
        j=0
        
        while(j<n):
            
            print(j)
            i = tups[j]
            if(i == 'b' and j<len(tups)-1):
                #do
                print('changing')
                tups[j],tups[-1] = tups[-1],tups[j]
                n=n-1
                #tups=tups[:len(tups)-1]
                cx = c % gs[0]
                cy = c // gs[0]
                
                continue
            c+=1
            x,y = self.tilepos[i]
            image = pic.subsurface(x,y,ts,ts)
            text = self.font.render(str(i+1),2,(255,255,255)); w,h = text.get_size()    #NUMBERS
            image.blit(text,((ts-w)/2,(ts-w)/2)); self.images+=[image]
            j+=1
            
        self.switch1((cx,cy)) #IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!
    
            
    def getBlank(self): return self.tiles[-1] #blank is always the last tile
    def setBlank(self,pos): self.tiles[-1]=pos
    
    opentile = property(getBlank,setBlank)
    
    def sliding(self):
        for i in range(self.tiles_len):
            x,y = self.tilepos[i] # current pos
            X,Y = self.tilePOS[self.tiles[i]] #target pos
            if x!=X or y!=Y: return True
            
    #this version of the switch doesn't modify the internal matrix
    
    def switch1(self,tile):
        if (not tile in self.tiles): return;
        
        if self.sliding(): return;
        
        self.sound.play()
        n = self.tiles.index(tile)
        x0,y0 = self.tiles[n]
        x1,y1 = self.opentile
        #self.matrix[y0][x0],self.matrix[y1][x1] = self.matrix[y1][x1],self.matrix[y0][x0]
        self.tiles[n],self.opentile,self.prev = self.opentile,self.tiles[n],self.opentile
    
    def switch(self,tile):
        if (not tile in self.tiles): return;
        
        if self.sliding(): return;
        
        self.sound.play()
        n = self.tiles.index(tile)
        x0,y0 = self.tiles[n]
        x1,y1 = self.opentile
        self.matrix[y0][x0],self.matrix[y1][x1] = self.matrix[y1][x1],self.matrix[y0][x0]
        self.tiles[n],self.opentile,self.prev = self.opentile,self.tiles[n],self.opentile
        
       
        
        
    def fast_switch(self,tile):
        if (not tile in self.tiles): return;
              
        n = self.tiles.index(tile)
        
        x0,y0 = self.tiles[n]
        x1,y1 = self.opentile
        self.matrix[y0][x0],self.matrix[y1][x1] = self.matrix[y1][x1],self.matrix[y0][x0]
        
        self.tiles[n],self.opentile,self.prev = self.opentile,self.tiles[n],self.opentile
        
    def in_grid(self,tile): return tile[0]>=0 and tile[0]<self.gs[0] and tile[1]>=0 and tile[0]<self.gs[1]
    
    def adjacent(self):
        x,y=self.opentile
        return (x-1,y),(x+1,y),(x,y-1),(x,y+1)
    
    def random(self):
        adj = self.adjacent()        
        self.fast_switch(random.choice([pos for pos in adj if self.in_grid(pos) and pos!=self.prev]))
        
        
    
    def update(self,dt):
        s = self.speed*dt
        mouse = pygame.mouse.get_pressed()
        mpos = pygame.mouse.get_pos()
        
        if mouse[0]:
            
            x,y = mpos[0]%(self.ts+self.ms),mpos[1]%(self.ts+self.ms)
            if x>self.ms and y>self.ms:
                tile = mpos[0]//self.ts,mpos[1]//self.ts
                if self.in_grid(tile) and tile in self.adjacent():                        
                        self.switch(tile)
                        print(tile)
            
            
            print(mpos)
            
        
        for i in range(self.tiles_len):
            x,y = self.tilepos[i] # current pos
            X,Y = self.tilePOS[self.tiles[i]] #target pos
            dx,dy = X-x,Y-y
            x = (X if abs(dx)<s  else x+s if dx>0 else x-s )
            y = (Y if abs(dy)<s  else y+s if dy>0 else y-s )
            self.tilepos[i] = x,y
            
            if abs(dx)<s: x = X
            elif dx>0: x+=s
            else: x = X
        
        
        
        # find tile mmouse is on. If helf, switch, as long
        #as the blank is adjecent
        
    
    def draw(self,screen):
        for i in range(self.tiles_len):
            x,y = self.tilepos[i]
            screen.blit(self.images[i],(x,y))
            
    def events(self,event):
        if event.type == pygame.KEYDOWN:
            for key,dx,dy in ((pygame.K_w,0,-1),(pygame.K_s,0,1),(pygame.K_a,-1,0),(pygame.K_d,1,0)):
                if event.key == key:
                    x,y = self.opentile; tile = x+dx,y+dy
                    if self.in_grid(tile): self.switch(tile)
                    
            if event.key == pygame.K_r:
                self.unleash = False
                for i in range(100): self.random()
                
            if event.key == pygame.K_p:
                self.unleash=True
                
            if event.key == pygame.K_m:
                
                text = open("output2.txt",'w')
                textb = open("output1.txt",'w')
                
                s = '1 2 3\n4 5 6\n7 8 0'
                text.write(s)
                for i in range(len(self.matrix)):
                    line =""
                    for j in range(len(self.matrix)):
                        line+=str(self.matrix[i][j])+" "
                    line = line[:-1]+"\n"
                    textb.write(line)
                
                textb.close()
                text.close()
                try:
                    os.remove("lispOutput.txt")
                    print("h")
                except OSError:
                    pass
                subprocess.call([r'load.bat'])
                self.reading = True
                self.unleash=True
                
                
            for l in self.matrix:
                print(l)
                
    def externalEvent(self,word):
        
        #for key,dx,dy in (('DOWN',0,-1),('UP',0,1),('RIGHT',-1,0),('LEFT',1,0)):
        for key,dx,dy in (('2',0,-1),('4',0,1),('1',-1,0),('3',1,0)):
            if word == key:
                x,y = self.opentile; tile = x+dx,y+dy
                if self.in_grid(tile): self.switch(tile)
                    
            
                
            
            
            
def readStack(file):
    
    with open(file, 'r') as file:
        
        data = file.read().replace('\n', ' ')
        res = data.split(' ')
        return res
    
        
def readStart(file):
    with open(file, 'r') as file:
        
        data = file.read().replace('\n', ' ')
        res = data.split(' ')
        res = res[:-1]
        for i in range(len(res)):
            if(res[i]!='0'):
                res[i]=int(res[i])-1
            else:
                res[i]='b'
        return res     


def main():
    ops = readStack("lispOutput.txt")
    numsS = readStart("output1.txt")
    tileh = 160
    margin = 5
    gs = 3
    l = tileh*gs+margin*3 + margin
    pygame.init()
    os.environ['SDL_VIDEO_CENTERED']='1'
    pygame.display.set_caption('Puzzle')
    pygame.display.set_mode((l,l))
    screen=pygame.display.set_mode((l,l))
    fpsclock = pygame.time.Clock()
    program = SlidePuzzle((gs,gs),tileh,margin,numsS)
    
    while True:
        dt=fpsclock.tick()/1000
        
        screen.fill((0,0,0))
        program.draw(screen)
        pygame.display.flip()
        
        for event in pygame.event.get():
            if event.type == pygame.QUIT: pygame.quit(); sys.exit();
            program.events(event)
            
        program.update(dt)
        
        if( (not program.sliding()) and ops !=[] and program.unleash):
            program.externalEvent(ops.pop(0))
        if(program.reading):
            ops = readStack("lispOutput.txt")
            program.reading=False
            
            
if __name__=='__main__':
    main()
    
    
