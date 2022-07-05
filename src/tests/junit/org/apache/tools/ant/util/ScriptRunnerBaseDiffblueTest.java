package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import junit.runner.TestCaseClassLoader;
import org.apache.ant.antunit.AntUnit;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.PropertyResource;
import org.apache.tools.ant.types.resources.StringResource;
import org.junit.Test;

public class ScriptRunnerBaseDiffblueTest {
  /**
  * Method under test: {@link ScriptRunnerBase#addBean(String, Object)}
  */
  @Test
  public void testAddBean() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.addBean("Key", "Bean");

    // Assert
    assertEquals(1, scriptRunner.getBeans().size());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#addBean(String, Object)}
   */
  @Test
  public void testAddBean2() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.addBean("", "Bean");

    // Assert that nothing has changed
    assertEquals("", scriptRunner.getScript());
    assertFalse(scriptRunner.getKeepEngine());
    assertFalse(scriptRunner.getCompiled());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#addBean(String, Object)}
   */
  @Test
  public void testAddBean3() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.addBean("42", "Bean");

    // Assert that nothing has changed
    assertEquals("", scriptRunner.getScript());
    assertFalse(scriptRunner.getKeepEngine());
    assertFalse(scriptRunner.getCompiled());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#addBean(String, Object)}
   */
  @Test
  public void testAddBean4() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.addBean("org.apache.tools.ant.types.resources.PropertyResource", "Bean");

    // Assert that nothing has changed
    assertEquals("", scriptRunner.getScript());
    assertFalse(scriptRunner.getKeepEngine());
    assertFalse(scriptRunner.getCompiled());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#addBeans(Map)}
   */
  @Test
  public void testAddBeans() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    HashMap<String, Object> stringObjectMap = new HashMap<>();
    stringObjectMap.put("foo", "42");

    // Act
    scriptRunner.addBeans(stringObjectMap);

    // Assert
    assertEquals(1, scriptRunner.getBeans().size());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#addBeans(Map)}
   */
  @Test
  public void testAddBeans2() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    HashMap<String, Object> stringObjectMap = new HashMap<>();
    stringObjectMap.put("42", "42");
    stringObjectMap.put("foo", "42");

    // Act
    scriptRunner.addBeans(stringObjectMap);

    // Assert
    assertEquals(1, scriptRunner.getBeans().size());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#addText(String)}
   */
  @Test
  public void testAddText() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.addText("Text");

    // Assert
    assertEquals("Text", scriptRunner.getScript());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#bindToComponentMinimum(ProjectComponent)}
   */
  @Test
  public void testBindToComponentMinimum() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.bindToComponentMinimum(new AntUnit());

    // Assert
    assertEquals(2, scriptRunner.getBeans().size());
    assertNull(scriptRunner.getProject());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#checkLanguage()}
   */
  @Test
  public void testCheckLanguage() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ScriptRunner()).checkLanguage());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#checkLanguage()}
   */
  @Test
  public void testCheckLanguage2() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setLanguage("en");

    // Act
    scriptRunner.checkLanguage();

    // Assert that nothing has changed
    assertEquals("", scriptRunner.getScript());
    assertEquals("en", scriptRunner.getLanguage());
    assertFalse(scriptRunner.getKeepEngine());
    assertFalse(scriptRunner.getCompiled());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#clearScript()}
   */
  @Test
  public void testClearScript() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.clearScript();

    // Assert
    assertEquals("", scriptRunner.getScript());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#getBeans()}
   */
  @Test
  public void testGetBeans() {
    // Arrange, Act and Assert
    assertTrue((new ScriptRunner()).getBeans().isEmpty());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#getCompiled()}
   */
  @Test
  public void testGetCompiled() {
    // Arrange, Act and Assert
    assertFalse((new ScriptRunner()).getCompiled());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#getCompiled()}
   */
  @Test
  public void testGetCompiled2() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setCompiled(true);

    // Act and Assert
    assertTrue(scriptRunner.getCompiled());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#getKeepEngine()}
   */
  @Test
  public void testGetKeepEngine() {
    // Arrange, Act and Assert
    assertFalse((new ScriptRunner()).getKeepEngine());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#getKeepEngine()}
   */
  @Test
  public void testGetKeepEngine2() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setKeepEngine(true);

    // Act and Assert
    assertTrue(scriptRunner.getKeepEngine());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#getLanguage()}
   */
  @Test
  public void testGetLanguage() {
    // Arrange, Act and Assert
    assertNull((new ScriptRunner()).getLanguage());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#getProject()}
   */
  @Test
  public void testGetProject() {
    // Arrange, Act and Assert
    assertNull((new ScriptRunner()).getProject());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#getScript()}
   */
  @Test
  public void testGetScript() {
    // Arrange, Act and Assert
    assertEquals("", (new ScriptRunner()).getScript());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#getScriptClassLoader()}
   */
  @Test
  public void testGetScriptClassLoader() {
    // Arrange, Act and Assert
    assertNull((new ScriptRunner()).getScriptClassLoader());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.loadResource(new Resource("Name")));
  }

  /**
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource2() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.loadResource(new StringResource("42"));

    // Assert
    assertEquals("42", scriptRunner.getScript());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource3() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.loadResource(new PropertyResource());

    // Assert
    assertEquals("null", scriptRunner.getScript());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource4() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    PropertyResource propertyResource = new PropertyResource(new Project(), " \"");

    // Act
    scriptRunner.loadResource(propertyResource);

    // Assert
    assertFalse(propertyResource.isExists());
    Project project = propertyResource.getProject();
    Hashtable<String, Object> userProperties = project.getUserProperties();
    assertTrue(userProperties.isEmpty());
    assertEquals(userProperties, project.getTaskDefinitions());
    Hashtable<String, Target> expectedInheritedProperties = project.getTargets();
    assertEquals(expectedInheritedProperties, project.getInheritedProperties());
    assertNull(project.getDescription());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource5() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    Resource resource = new Resource("Name");
    resource.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.loadResource(resource));
  }

  /**
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource6() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    PropertyResource propertyResource = new PropertyResource(project, " \"");

    // Act
    scriptRunner.loadResource(propertyResource);

    // Assert
    assertFalse(propertyResource.isExists());
    Project project1 = propertyResource.getProject();
    Hashtable<String, Object> userProperties = project1.getUserProperties();
    assertTrue(userProperties.isEmpty());
    assertEquals(userProperties, project1.getTaskDefinitions());
    Hashtable<String, Target> expectedInheritedProperties = project1.getTargets();
    assertEquals(expectedInheritedProperties, project1.getInheritedProperties());
    assertNull(project1.getDescription());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.loadResources(new Resource(" \"")));
  }

  /**
   * Method under test: {@link ScriptRunnerBase#replaceContextLoader()}
   */
  @Test
  public void testReplaceContextLoader() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    ClassLoader actualReplaceContextLoaderResult = scriptRunner.replaceContextLoader();

    // Assert
    assertSame(actualReplaceContextLoaderResult, scriptRunner.getScriptClassLoader());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#setCompiled(boolean)}
   */
  @Test
  public void testSetCompiled() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.setCompiled(true);

    // Assert
    assertTrue(scriptRunner.getCompiled());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#setKeepEngine(boolean)}
   */
  @Test
  public void testSetKeepEngine() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.setKeepEngine(true);

    // Assert
    assertTrue(scriptRunner.getKeepEngine());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#setLanguage(String)}
   */
  @Test
  public void testSetLanguage() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act
    scriptRunner.setLanguage("en");

    // Assert
    assertEquals("en", scriptRunner.getLanguage());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#setScriptClassLoader(ClassLoader)}
   */
  @Test
  public void testSetScriptClassLoader() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    TestCaseClassLoader testCaseClassLoader = new TestCaseClassLoader();

    // Act
    scriptRunner.setScriptClassLoader(testCaseClassLoader);

    // Assert
    assertSame(testCaseClassLoader, scriptRunner.getScriptClassLoader());
  }

  /**
   * Method under test: {@link ScriptRunnerBase#setSrc(File)}
   */
  @Test
  public void testSetSrc() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> scriptRunner.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link ScriptRunnerBase#setSrc(File)}
   */
  @Test
  public void testSetSrc2() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> scriptRunner.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "").toFile()));
  }

  /**
   * Method under test: {@link ScriptRunnerBase#setSrc(File)}
   */
  @Test
  public void testSetSrc3() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setEncoding("UTF-8");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> scriptRunner.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "").toFile()));
  }
}

