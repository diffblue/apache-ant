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
import java.util.Map;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.PropertyResource;
import org.apache.tools.ant.types.resources.Resources;
import org.apache.tools.ant.types.resources.StringResource;
import org.apache.tools.ant.util.optional.JavaxScriptRunner;
import org.junit.Test;

public class ScriptRunnerBaseDiffblueTest {
  /**
   * Test {@link ScriptRunnerBase#addBeans(Map)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link HashMap#HashMap()} empty string is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#addBeans(Map)}
   */
  @Test
  public void testAddBeans_givenEmptyString_whenHashMapEmptyStringIs42() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    HashMap<String, Object> dictionary = new HashMap<>();
    dictionary.put("", "42");

    // Act
    javaxScriptRunner.addBeans(dictionary);

    // Assert that nothing has changed
    assertTrue(javaxScriptRunner.getBeans().isEmpty());
  }

  /**
   * Test {@link ScriptRunnerBase#addBeans(Map)}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link HashMap#HashMap()} {@code 42} is {@code 42}.</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Beans size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#addBeans(Map)}
   */
  @Test
  public void testAddBeans_givenFoo_whenHashMap42Is42_thenJavaxScriptRunnerBeansSizeIsOne() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    HashMap<String, Object> dictionary = new HashMap<>();
    dictionary.put("42", "42");
    dictionary.put("foo", "42");

    // Act
    javaxScriptRunner.addBeans(dictionary);

    // Assert
    Map<String, Object> beans = javaxScriptRunner.getBeans();
    assertEquals(1, beans.size());
    assertEquals("42", beans.get("foo"));
  }

  /**
   * Test {@link ScriptRunnerBase#addBeans(Map)}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link HashMap#HashMap()} {@code foo} is {@code 42}.</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Beans is {@link HashMap#HashMap()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#addBeans(Map)}
   */
  @Test
  public void testAddBeans_givenFoo_whenHashMapFooIs42_thenJavaxScriptRunnerBeansIsHashMap() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    HashMap<String, Object> dictionary = new HashMap<>();
    dictionary.put("foo", "42");

    // Act
    javaxScriptRunner.addBeans(dictionary);

    // Assert
    assertEquals(dictionary, javaxScriptRunner.getBeans());
  }

  /**
   * Test {@link ScriptRunnerBase#addBeans(Map)}.
   * <ul>
   *   <li>When {@link HashMap#HashMap()}.</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Beans Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#addBeans(Map)}
   */
  @Test
  public void testAddBeans_whenHashMap_thenJavaxScriptRunnerBeansEmpty() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.addBeans(new HashMap<>());

    // Assert that nothing has changed
    assertTrue(javaxScriptRunner.getBeans().isEmpty());
  }

  /**
   * Test {@link ScriptRunnerBase#addBean(String, Object)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Beans Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#addBean(String, Object)}
   */
  @Test
  public void testAddBean_when42_thenJavaxScriptRunnerBeansEmpty() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.addBean("42", "Bean");

    // Assert that nothing has changed
    assertTrue(javaxScriptRunner.getBeans().isEmpty());
  }

  /**
   * Test {@link ScriptRunnerBase#addBean(String, Object)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Beans Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#addBean(String, Object)}
   */
  @Test
  public void testAddBean_whenEmptyString_thenJavaxScriptRunnerBeansEmpty() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.addBean("", "Bean");

    // Assert that nothing has changed
    assertTrue(javaxScriptRunner.getBeans().isEmpty());
  }

  /**
   * Test {@link ScriptRunnerBase#addBean(String, Object)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Beans size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#addBean(String, Object)}
   */
  @Test
  public void testAddBean_whenKey_thenJavaxScriptRunnerBeansSizeIsOne() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.addBean("Key", "Bean");

    // Assert
    Map<String, Object> beans = javaxScriptRunner.getBeans();
    assertEquals(1, beans.size());
    assertEquals("Bean", beans.get("Key"));
  }

  /**
   * Test {@link ScriptRunnerBase#addBean(String, Object)}.
   * <ul>
   *   <li>When {@code PropertyResource}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#addBean(String, Object)}
   */
  @Test
  public void testAddBean_whenOrgApacheToolsAntTypesResourcesPropertyResource() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.addBean("org.apache.tools.ant.types.resources.PropertyResource", "Bean");

    // Assert that nothing has changed
    assertTrue(javaxScriptRunner.getBeans().isEmpty());
  }

  /**
   * Test {@link ScriptRunnerBase#getBeans()}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#getBeans()}
   */
  @Test
  public void testGetBeans() {
    // Arrange, Act and Assert
    assertTrue((new JavaxScriptRunner()).getBeans().isEmpty());
  }

  /**
   * Test {@link ScriptRunnerBase#setLanguage(String)}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#setLanguage(String)}
   */
  @Test
  public void testSetLanguage() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.setLanguage("en");

    // Assert
    assertEquals("en", javaxScriptRunner.getLanguage());
  }

  /**
   * Test {@link ScriptRunnerBase#getLanguage()}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#getLanguage()}
   */
  @Test
  public void testGetLanguage() {
    // Arrange, Act and Assert
    assertNull((new JavaxScriptRunner()).getLanguage());
  }

  /**
   * Test {@link ScriptRunnerBase#setScriptClassLoader(ClassLoader)}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#setScriptClassLoader(ClassLoader)}
   */
  @Test
  public void testSetScriptClassLoader() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    AntClassLoader classLoader = new AntClassLoader();

    // Act
    javaxScriptRunner.setScriptClassLoader(classLoader);

    // Assert
    assertSame(classLoader, javaxScriptRunner.getScriptClassLoader());
  }

  /**
   * Test {@link ScriptRunnerBase#getScriptClassLoader()}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#getScriptClassLoader()}
   */
  @Test
  public void testGetScriptClassLoader() {
    // Arrange, Act and Assert
    assertNull((new JavaxScriptRunner()).getScriptClassLoader());
  }

  /**
   * Test {@link ScriptRunnerBase#setKeepEngine(boolean)}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#setKeepEngine(boolean)}
   */
  @Test
  public void testSetKeepEngine() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.setKeepEngine(true);

    // Assert
    assertTrue(javaxScriptRunner.getKeepEngine());
  }

  /**
   * Test {@link ScriptRunnerBase#getKeepEngine()}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor) KeepEngine is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#getKeepEngine()}
   */
  @Test
  public void testGetKeepEngine_givenJavaxScriptRunnerKeepEngineIsTrue_thenReturnTrue() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setKeepEngine(true);

    // Act and Assert
    assertTrue(javaxScriptRunner.getKeepEngine());
  }

  /**
   * Test {@link ScriptRunnerBase#getKeepEngine()}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#getKeepEngine()}
   */
  @Test
  public void testGetKeepEngine_givenJavaxScriptRunner_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new JavaxScriptRunner()).getKeepEngine());
  }

  /**
   * Test {@link ScriptRunnerBase#setCompiled(boolean)}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#setCompiled(boolean)}
   */
  @Test
  public void testSetCompiled() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.setCompiled(true);

    // Assert
    assertTrue(javaxScriptRunner.getCompiled());
  }

  /**
   * Test {@link ScriptRunnerBase#getCompiled()}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor) Compiled is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#getCompiled()}
   */
  @Test
  public void testGetCompiled_givenJavaxScriptRunnerCompiledIsTrue_thenReturnTrue() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setCompiled(true);

    // Act and Assert
    assertTrue(javaxScriptRunner.getCompiled());
  }

  /**
   * Test {@link ScriptRunnerBase#getCompiled()}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#getCompiled()}
   */
  @Test
  public void testGetCompiled_givenJavaxScriptRunner_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new JavaxScriptRunner()).getCompiled());
  }

  /**
   * Test {@link ScriptRunnerBase#setSrc(File)}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor) Encoding is {@code UTF-8}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#setSrc(File)}
   */
  @Test
  public void testSetSrc_givenJavaxScriptRunnerEncodingIsUtf8_thenThrowBuildException() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setEncoding("UTF-8");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> javaxScriptRunner.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ScriptRunnerBase#setSrc(File)}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#setSrc(File)}
   */
  @Test
  public void testSetSrc_givenJavaxScriptRunner_thenThrowBuildException() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> javaxScriptRunner.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ScriptRunnerBase#setSrc(File)}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor).</li>
   *   <li>When Property is {@code java.io.tmpdir} is {@code Failed to read} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#setSrc(File)}
   */
  @Test
  public void testSetSrc_givenJavaxScriptRunner_whenPropertyIsJavaIoTmpdirIsFailedToReadToFile() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> javaxScriptRunner.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "Failed to read ").toFile()));
  }

  /**
   * Test {@link ScriptRunnerBase#loadResource(Resource)}.
   * <ul>
   *   <li>Given {@code Adding reference:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource_givenAddingReference() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("Adding reference: ", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act
    javaxScriptRunner.loadResource(new PropertyResource(p, " \""));

    // Assert
    assertEquals("null", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResource(Resource)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act
    javaxScriptRunner.loadResource(new PropertyResource(p, " \""));

    // Assert
    assertEquals("null", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResource(Resource)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>When {@link PropertyResource#PropertyResource()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource_givenNull_whenPropertyResourceProjectIsNull() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    PropertyResource sourceResource = new PropertyResource();
    sourceResource.setProject(null);

    // Act
    javaxScriptRunner.loadResource(sourceResource);

    // Assert
    assertEquals("null", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResource(Resource)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link PropertyResource#PropertyResource()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource_givenProject_whenPropertyResourceProjectIsProject() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    PropertyResource sourceResource = new PropertyResource();
    sourceResource.setProject(new Project());

    // Act
    javaxScriptRunner.loadResource(sourceResource);

    // Assert
    assertEquals("null", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResource(Resource)}.
   * <ul>
   *   <li>When {@link PropertyResource#PropertyResource(Project, String)} with p is {@link Project} (default constructor) and n is {@code "}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource_whenPropertyResourceWithPIsProjectAndNIsQuotationMark() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.loadResource(new PropertyResource(new Project(), " \""));

    // Assert
    assertEquals("null", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResource(Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource_whenResourceProjectIsProject_thenThrowBuildException() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Resource sourceResource = new Resource();
    sourceResource.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.loadResource(sourceResource));
  }

  /**
   * Test {@link ScriptRunnerBase#loadResource(Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource_whenResource_thenThrowBuildException() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.loadResource(new Resource()));
  }

  /**
   * Test {@link ScriptRunnerBase#loadResource(Resource)}.
   * <ul>
   *   <li>When {@link StringResource#StringResource()}.</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Script is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResource(Resource)}
   */
  @Test
  public void testLoadResource_whenStringResource_thenJavaxScriptRunnerScriptIsNull() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.loadResource(new StringResource());

    // Assert
    assertEquals("null", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.loadResources(new Path(p, "Path")));
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   *   <li>When {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_givenFileList_whenConcatAddFilelistFileList() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Concat collection = new Concat();
    collection.addFilelist(new FileList());

    // Act
    javaxScriptRunner.loadResources(collection);

    // Assert that nothing has changed
    assertEquals("", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_givenJavaLangObject() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("Users", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.loadResources(new Path(p, "Path")));
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>Given {@code FileResource}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_givenOrgApacheToolsAntTypesResourcesFileResource() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Project p = new Project();
    Class<FileResource> typeClass = FileResource.class;
    p.addDataTypeDefinition("Users", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.loadResources(new Path(p, "Path")));
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>Given {@code Text}.</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Script is {@code Text}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_givenText_thenJavaxScriptRunnerScriptIsText() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Concat collection = new Concat();
    collection.addText("Text");

    // Act
    javaxScriptRunner.loadResources(collection);

    // Assert
    assertEquals("Text", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Concat} (default constructor) Binary is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_givenTrue_whenConcatBinaryIsTrue() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Concat collection = new Concat();
    collection.setBinary(true);
    collection.addFilelist(new FileList());

    // Act
    javaxScriptRunner.loadResources(collection);

    // Assert that nothing has changed
    assertEquals("", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Script is {@code Text}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_whenConcatProjectIsProject_thenJavaxScriptRunnerScriptIsText() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Concat collection = new Concat();
    collection.setProject(new Project());
    collection.addText("Text");

    // Act
    javaxScriptRunner.loadResources(collection);

    // Assert
    assertEquals("Text", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Script is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_whenNone_thenJavaxScriptRunnerScriptIsEmptyString() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.loadResources(Resources.NONE);

    // Assert that nothing has changed
    assertEquals("", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_whenPathWithPIsProjectAndPathIsDot_thenThrowBuildException() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.loadResources(new Path(new Project(), ".")));
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_whenPathWithPIsProjectAndPath_thenThrowBuildException() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.loadResources(new Path(new Project(), "Path")));
  }

  /**
   * Test {@link ScriptRunnerBase#loadResources(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#loadResources(ResourceCollection)}
   */
  @Test
  public void testLoadResources_whenResource_thenThrowBuildException() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.loadResources(new Resource()));
  }

  /**
   * Test {@link ScriptRunnerBase#addText(String)}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#addText(String)}
   */
  @Test
  public void testAddText() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    // Act
    javaxScriptRunner.addText("Text");

    // Assert
    assertEquals("Text", javaxScriptRunner.getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#getScript()}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#getScript()}
   */
  @Test
  public void testGetScript() {
    // Arrange, Act and Assert
    assertEquals("", (new JavaxScriptRunner()).getScript());
  }

  /**
   * Test {@link ScriptRunnerBase#setProject(Project)}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    Project project = new Project();

    // Act
    javaxScriptRunner.setProject(project);

    // Assert
    assertSame(project, javaxScriptRunner.getProject());
  }

  /**
   * Test {@link ScriptRunnerBase#getProject()}.
   * <p>
   * Method under test: {@link ScriptRunnerBase#getProject()}
   */
  @Test
  public void testGetProject() {
    // Arrange, Act and Assert
    assertNull((new JavaxScriptRunner()).getProject());
  }

  /**
   * Test {@link ScriptRunnerBase#bindToComponent(ProjectComponent)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#bindToComponent(ProjectComponent)}
   */
  @Test
  public void testBindToComponent_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter component = new TaskAdapter();
    component.setProject(project);

    // Act
    javaxScriptRunner.bindToComponent(component);

    // Assert
    Map<String, Object> beans = javaxScriptRunner.getBeans();
    assertEquals(2, beans.size());
    assertSame(project, beans.get("project"));
    assertSame(project, javaxScriptRunner.getProject());
    assertSame(component, beans.get("self"));
  }

  /**
   * Test {@link ScriptRunnerBase#bindToComponent(ProjectComponent)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#bindToComponent(ProjectComponent)}
   */
  @Test
  public void testBindToComponent_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    TaskAdapter component = new TaskAdapter();
    component.setProject(project);

    // Act
    javaxScriptRunner.bindToComponent(component);

    // Assert
    Map<String, Object> beans = javaxScriptRunner.getBeans();
    assertEquals(2, beans.size());
    assertSame(project, beans.get("project"));
    assertSame(project, javaxScriptRunner.getProject());
    assertSame(component, beans.get("self"));
  }

  /**
   * Test {@link ScriptRunnerBase#bindToComponent(ProjectComponent)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Beans size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#bindToComponent(ProjectComponent)}
   */
  @Test
  public void testBindToComponent_givenProject_thenJavaxScriptRunnerBeansSizeIsTwo() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();

    TaskAdapter component = new TaskAdapter();
    Project project = new Project();
    component.setProject(project);

    // Act
    javaxScriptRunner.bindToComponent(component);

    // Assert
    Map<String, Object> beans = javaxScriptRunner.getBeans();
    assertEquals(2, beans.size());
    assertSame(project, beans.get("project"));
    assertSame(project, javaxScriptRunner.getProject());
    assertSame(component, beans.get("self"));
  }

  /**
   * Test {@link ScriptRunnerBase#bindToComponentMinimum(ProjectComponent)}.
   * <ul>
   *   <li>Then {@link JavaxScriptRunner} (default constructor) Beans size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#bindToComponentMinimum(ProjectComponent)}
   */
  @Test
  public void testBindToComponentMinimum_thenJavaxScriptRunnerBeansSizeIsTwo() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    Path component = Path.systemBootClasspath;

    // Act
    javaxScriptRunner.bindToComponentMinimum(component);

    // Assert
    Map<String, Object> beans = javaxScriptRunner.getBeans();
    assertEquals(2, beans.size());
    Path expectedGetResult = component.systemBootClasspath;
    assertSame(expectedGetResult, beans.get("self"));
  }

  /**
   * Test {@link ScriptRunnerBase#checkLanguage()}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerBase#checkLanguage()}
   */
  @Test
  public void testCheckLanguage_givenJavaxScriptRunner_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JavaxScriptRunner()).checkLanguage());
  }
}
