
"""
Simple proof of concept program to show how we traverse the testing hierarchy
of an XML testing namelist

Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com

"""
import xml.etree.ElementTree as ET

tree = ET.parse('stuff2.xml')
root = tree.getroot()

print 'root tag and attrib: '
print root.tag, root.attrib

print 'Distribution variables:'
metcase_list = []
for child in root:
    if child.text.strip():
        print child.tag, child.text.strip()
    if child.tag == "metcase":
        print 'in metcase'
        metcasechildren = child.getchildren()
        print 
        print 'metcase:'
        for m in metcasechildren:
            print m.tag, m.text.strip()

            if m.tag == 'runcase':        
                runcasechildren = m.getchildren() 
                print
                print 'in runcase:'
                for r in runcasechildren:
                    print r.tag, r.text.strip()

                    if r.tag == 'basictest':
                        basictestchildren = r.getchildren()
                        print
                        print 'in basictest'
                        for b in basictestchildren:
                           print b.tag, b.text.strip()

'''
print 'tree.iter():'
for elem in tree.iter():
    print elem.tag, elem.text
'''







