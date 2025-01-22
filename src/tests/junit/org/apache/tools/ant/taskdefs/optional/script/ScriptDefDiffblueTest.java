package org.apache.tools.ant.taskdefs.optional.script;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Hashtable;
import java.util.Map;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.AntTypeDefinition;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.ComponentHelper;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.script.ScriptDef.Attribute;
import org.apache.tools.ant.taskdefs.optional.script.ScriptDef.NestedElement;
import org.junit.Test;

public class ScriptDefDiffblueTest {
  /**
   * Test Attribute getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Attribute}
   *   <li>{@link Attribute#setDefault(String)}
   *   <li>{@link Attribute#getDefault()}
   *   <li>{@link Attribute#getName()}
   * </ul>
   */
  @Test
  public void testAttributeGettersAndSetters() {
    // Arrange and Act
    Attribute actualAttribute = new Attribute();
    actualAttribute.setDefault("42");
    String actualDefault = actualAttribute.getDefault();

    // Assert
    assertEquals("42", actualDefault);
    assertNull(actualAttribute.getName());
  }

  /**
   * Test Attribute {@link Attribute#hasDefault()}.
   * <ul>
   *   <li>Given {@link Attribute} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#hasDefault()}
   */
  @Test
  public void testAttributeHasDefault_givenAttributeNameIsName_thenReturnTrue() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setName("Name");
    attribute.setDefault("foo");

    // Act and Assert
    assertTrue(attribute.hasDefault());
  }

  /**
   * Test Attribute {@link Attribute#hasDefault()}.
   * <ul>
   *   <li>Given {@link Attribute} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#hasDefault()}
   */
  @Test
  public void testAttributeHasDefault_givenAttribute_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Attribute()).hasDefault());
  }

  /**
   * Test Attribute {@link Attribute#setName(String)}.
   * <p>
   * Method under test: {@link Attribute#setName(String)}
   */
  @Test
  public void testAttributeSetName() {
    // Arrange
    Attribute attribute = new Attribute();

    // Act
    attribute.setName("Name");

    // Assert
    assertEquals("name", attribute.getName());
  }

  /**
   * Test new {@link ScriptDef} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ScriptDef}
   */
  @Test
  public void testNewScriptDef() {
    // Arrange and Act
    ScriptDef actualScriptDef = new ScriptDef();

    // Assert
    assertEquals("", actualScriptDef.getURI());
    assertNull(actualScriptDef.getAntlibClassLoader());
    assertNull(actualScriptDef.getDescription());
    assertNull(actualScriptDef.getTaskName());
    assertNull(actualScriptDef.getTaskType());
    assertNull(actualScriptDef.getClasspathId());
    assertNull(actualScriptDef.getLoaderId());
    assertNull(actualScriptDef.getProject());
    assertNull(actualScriptDef.getOwningTarget());
    assertNull(actualScriptDef.getClasspath());
    assertFalse(actualScriptDef.isReverseLoader());
  }

  /**
   * Test {@link ScriptDef#setProject(Project)}.
   * <p>
   * Method under test: {@link ScriptDef#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    ScriptDef scriptDef = new ScriptDef();
    Project project = new Project();

    // Act
    scriptDef.setProject(project);

    // Assert
    assertSame(project, scriptDef.getProject());
  }

  /**
   * Test {@link ScriptDef#execute()}.
   * <p>
   * Method under test: {@link ScriptDef#execute()}
   */
  @Test
  public void testExecute() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("");
    attribute.setName("");

    NestedElement nestedElement = new NestedElement();
    nestedElement.setClassName("Class Name");
    nestedElement.setName("Name");
    nestedElement.setType("Type");

    Attribute attribute2 = new Attribute();
    attribute2.setDefault("scriptdef <element> elements must specify only one of the classname and type attributes");
    attribute2.setName("");

    ScriptDef scriptDef = new ScriptDef();
    scriptDef.addAttribute(attribute2);
    scriptDef.addElement(nestedElement);
    scriptDef.addAttribute(attribute);
    scriptDef.setLanguage("en");
    scriptDef.setName("scriptdef requires a name attribute to name the script");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptDef.execute());
  }

  /**
   * Test {@link ScriptDef#execute()}.
   * <ul>
   *   <li>Given {@link NestedElement} (default constructor) ClassName is {@code Class Name}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptDef#execute()}
   */
  @Test
  public void testExecute_givenNestedElementClassNameIsClassName_thenThrowBuildException() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("");
    attribute.setName("");

    NestedElement nestedElement = new NestedElement();
    nestedElement.setClassName("Class Name");
    nestedElement.setName("Name");
    nestedElement.setType("Type");

    ScriptDef scriptDef = new ScriptDef();
    scriptDef.addElement(nestedElement);
    scriptDef.addAttribute(attribute);
    scriptDef.setLanguage("en");
    scriptDef.setName("scriptdef requires a name attribute to name the script");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptDef.execute());
  }

  /**
   * Test {@link ScriptDef#execute()}.
   * <ul>
   *   <li>Given {@link ScriptDef} (default constructor) Encoding is {@code UTF-8}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptDef#execute()}
   */
  @Test
  public void testExecute_givenScriptDefEncodingIsUtf8_thenThrowBuildException() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("");
    attribute.setName("");

    ScriptDef scriptDef = new ScriptDef();
    scriptDef.setEncoding("UTF-8");
    scriptDef.addAttribute(attribute);
    scriptDef.setLanguage("en");
    scriptDef.setName("scriptdef requires a name attribute to name the script");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptDef.execute());
  }

  /**
   * Test {@link ScriptDef#execute()}.
   * <ul>
   *   <li>Given {@link ScriptDef} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptDef#execute()}
   */
  @Test
  public void testExecute_givenScriptDef_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ScriptDef()).execute());
  }

  /**
   * Test {@link ScriptDef#execute()}.
   * <ul>
   *   <li>Then {@link ScriptDef} (default constructor) Project CopyOfReferences size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptDef#execute()}
   */
  @Test
  public void testExecute_thenScriptDefProjectCopyOfReferencesSizeIsThree() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("");
    attribute.setName("");

    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    ScriptDef scriptDef = new ScriptDef();
    scriptDef.setProject(project);
    scriptDef.addAttribute(attribute);
    scriptDef.setLanguage("en");
    scriptDef.setName("scriptdef requires a name attribute to name the script");

    // Act
    scriptDef.execute();

    // Assert
    Project project2 = scriptDef.getProject();
    Map<String, Object> copyOfReferences = project2.getCopyOfReferences();
    assertEquals(3, copyOfReferences.size());
    Object getResult = copyOfReferences.get("org.apache.ant.scriptrepo");
    assertTrue(getResult instanceof Map);
    Object getResult2 = copyOfReferences.get("ant.ComponentHelper");
    assertTrue(getResult2 instanceof ComponentHelper);
    Hashtable<String, AntTypeDefinition> antTypeTable = ((ComponentHelper) getResult2).getAntTypeTable();
    assertEquals(1, antTypeTable.size());
    AntTypeDefinition getResult3 = antTypeTable.get("scriptdef requires a name attribute to name the script");
    assertEquals("org.apache.tools.ant.taskdefs.optional.script.ScriptDefBase", getResult3.getClassName());
    assertEquals("scriptdef requires a name attribute to name the script", getResult3.getName());
    Hashtable<String, Class<?>> taskDefinitions = project2.getTaskDefinitions();
    assertEquals(1, taskDefinitions.size());
    Map<String, Class<?>> copyOfTaskDefinitions = project2.getCopyOfTaskDefinitions();
    assertEquals(1, copyOfTaskDefinitions.size());
    assertEquals(1, ((Map<String, ScriptDef>) getResult).size());
    assertFalse(getResult3.isRestrict());
    assertTrue(taskDefinitions.containsKey("scriptdef requires a name attribute to name the script"));
    assertTrue(copyOfReferences.containsKey("ant.PropertyHelper"));
    assertEquals(copyOfReferences, project2.getReferences());
    Class<ScriptDefBase> expectedGetResult = ScriptDefBase.class;
    assertEquals(expectedGetResult,
        copyOfTaskDefinitions.get("scriptdef requires a name attribute to name the script"));
    assertSame(scriptDef,
        ((Map<String, ScriptDef>) getResult).get("scriptdef requires a name attribute to name the script"));
  }

  /**
   * Test {@link ScriptDef#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptDef#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() {
    // Arrange
    ScriptDef scriptDef = new ScriptDef();
    scriptDef.setName("scriptdef requires a name attribute to name the script");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptDef.execute());
  }
}
