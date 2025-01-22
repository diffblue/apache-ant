package org.apache.tools.ant;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.Map;
import java.util.Vector;
import org.apache.tools.ant.helper.DefaultExecutor;
import org.apache.tools.ant.helper.SingleCheckExecutor;
import org.apache.tools.ant.input.DefaultInputHandler;
import org.apache.tools.ant.input.InputHandler;
import org.apache.tools.ant.taskdefs.Execute;
import org.apache.tools.ant.types.FilterSet;
import org.apache.tools.ant.types.FilterSet.OnMissing;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class ProjectDiffblueTest {
  /**
   * Test {@link Project#createSubProject()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createSubProject()}
   */
  @Test
  public void testCreateSubProject_givenJavaLangObject_thenReturnDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(ComponentHelper.COMPONENT_HELPER_REFERENCE, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act
    Project actualCreateSubProjectResult = project.createSubProject();

    // Assert
    Hashtable<String, Class<?>> dataTypeDefinitions = actualCreateSubProjectResult.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualCreateSubProjectResult.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get(ComponentHelper.COMPONENT_HELPER_REFERENCE));
    assertSame(typeClass, dataTypeDefinitions.get(ComponentHelper.COMPONENT_HELPER_REFERENCE));
  }

  /**
   * Test {@link Project#createSubProject()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createSubProject()}
   */
  @Test
  public void testCreateSubProject_givenProjectCoreLoaderIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    project.addBuildListener(new AntClassLoader());

    // Act
    Project actualCreateSubProjectResult = project.createSubProject();

    // Assert
    assertFalse(actualCreateSubProjectResult.isKeepGoingMode());
    assertTrue(actualCreateSubProjectResult.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualCreateSubProjectResult.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getFilters());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getTargets());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getUserProperties());
  }

  /**
   * Test {@link Project#createSubProject()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) KeepGoingMode is {@code true}.</li>
   *   <li>Then return KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createSubProject()}
   */
  @Test
  public void testCreateSubProject_givenProjectKeepGoingModeIsTrue_thenReturnKeepGoingMode() {
    // Arrange
    Project project = new Project();
    project.setKeepGoingMode(true);
    project.addBuildListener(new AntClassLoader());

    // Act
    Project actualCreateSubProjectResult = project.createSubProject();

    // Assert
    assertTrue(actualCreateSubProjectResult.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualCreateSubProjectResult.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(actualCreateSubProjectResult.isKeepGoingMode());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getFilters());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getTargets());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getUserProperties());
  }

  /**
   * Test {@link Project#createSubProject()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then return not KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createSubProject()}
   */
  @Test
  public void testCreateSubProject_givenProject_thenReturnNotKeepGoingMode() {
    // Arrange and Act
    Project actualCreateSubProjectResult = (new Project()).createSubProject();

    // Assert
    assertFalse(actualCreateSubProjectResult.isKeepGoingMode());
    assertTrue(actualCreateSubProjectResult.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualCreateSubProjectResult.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getFilters());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getTargets());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getUserProperties());
  }

  /**
   * Test {@link Project#createSubProject()}.
   * <ul>
   *   <li>Then return not KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createSubProject()}
   */
  @Test
  public void testCreateSubProject_thenReturnNotKeepGoingMode() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    Project actualCreateSubProjectResult = project.createSubProject();

    // Assert
    assertFalse(actualCreateSubProjectResult.isKeepGoingMode());
    assertTrue(actualCreateSubProjectResult.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualCreateSubProjectResult.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getFilters());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getTargets());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, actualCreateSubProjectResult.getUserProperties());
  }

  /**
   * Test {@link Project#initSubProject(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then {@link Project} (default constructor) DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#initSubProject(Project)}
   */
  @Test
  public void testInitSubProject_givenJavaLangObject_thenProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());
    Project subProject = new Project();

    // Act
    project.initSubProject(subProject);

    // Assert
    Hashtable<String, Class<?>> dataTypeDefinitions = subProject.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = subProject.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertTrue(subProject.getFilters().isEmpty());
    assertTrue(subProject.getInheritedProperties().isEmpty());
    assertTrue(subProject.getTargets().isEmpty());
    assertTrue(subProject.getTaskDefinitions().isEmpty());
    assertTrue(subProject.getUserProperties().isEmpty());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get(MagicNames.REFID_PROPERTY_HELPER));
    assertSame(typeClass, dataTypeDefinitions.get(MagicNames.REFID_PROPERTY_HELPER));
  }

  /**
   * Test {@link Project#initSubProject(Project)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#initSubProject(Project)}
   */
  @Test
  public void testInitSubProject_givenProjectCoreLoaderIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    project.addBuildListener(new AntClassLoader());
    Project subProject = new Project();

    // Act
    project.initSubProject(subProject);

    // Assert that nothing has changed
    assertFalse(subProject.isKeepGoingMode());
    assertTrue(subProject.getDataTypeDefinitions().isEmpty());
    assertTrue(subProject.getFilters().isEmpty());
    assertTrue(subProject.getInheritedProperties().isEmpty());
    assertTrue(subProject.getTargets().isEmpty());
    assertTrue(subProject.getTaskDefinitions().isEmpty());
    assertTrue(subProject.getUserProperties().isEmpty());
    assertTrue(subProject.getCopyOfDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#initSubProject(Project)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) KeepGoingMode is {@code true}.</li>
   *   <li>Then {@link Project} (default constructor) KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#initSubProject(Project)}
   */
  @Test
  public void testInitSubProject_givenProjectKeepGoingModeIsTrue_thenProjectKeepGoingMode() {
    // Arrange
    Project project = new Project();
    project.setKeepGoingMode(true);
    project.addBuildListener(new AntClassLoader());
    Project subProject = new Project();

    // Act
    project.initSubProject(subProject);

    // Assert
    assertTrue(subProject.getDataTypeDefinitions().isEmpty());
    assertTrue(subProject.getFilters().isEmpty());
    assertTrue(subProject.getInheritedProperties().isEmpty());
    assertTrue(subProject.getTargets().isEmpty());
    assertTrue(subProject.getTaskDefinitions().isEmpty());
    assertTrue(subProject.getUserProperties().isEmpty());
    assertTrue(subProject.getCopyOfDataTypeDefinitions().isEmpty());
    assertTrue(subProject.isKeepGoingMode());
  }

  /**
   * Test {@link Project#initSubProject(Project)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then not {@link Project} (default constructor) KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#initSubProject(Project)}
   */
  @Test
  public void testInitSubProject_givenProject_whenProject_thenNotProjectKeepGoingMode() {
    // Arrange
    Project project = new Project();
    Project subProject = new Project();

    // Act
    project.initSubProject(subProject);

    // Assert that nothing has changed
    assertFalse(subProject.isKeepGoingMode());
    assertTrue(subProject.getDataTypeDefinitions().isEmpty());
    assertTrue(subProject.getFilters().isEmpty());
    assertTrue(subProject.getInheritedProperties().isEmpty());
    assertTrue(subProject.getTargets().isEmpty());
    assertTrue(subProject.getTaskDefinitions().isEmpty());
    assertTrue(subProject.getUserProperties().isEmpty());
    assertTrue(subProject.getCopyOfDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#initSubProject(Project)}.
   * <ul>
   *   <li>Then not {@link Project} (default constructor) KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#initSubProject(Project)}
   */
  @Test
  public void testInitSubProject_thenNotProjectKeepGoingMode() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    Project subProject = new Project();

    // Act
    project.initSubProject(subProject);

    // Assert that nothing has changed
    assertFalse(subProject.isKeepGoingMode());
    assertTrue(subProject.getDataTypeDefinitions().isEmpty());
    assertTrue(subProject.getFilters().isEmpty());
    assertTrue(subProject.getInheritedProperties().isEmpty());
    assertTrue(subProject.getTargets().isEmpty());
    assertTrue(subProject.getTaskDefinitions().isEmpty());
    assertTrue(subProject.getUserProperties().isEmpty());
    assertTrue(subProject.getCopyOfDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#createClassLoader(ClassLoader, Path)} with {@code parent}, {@code path}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(ClassLoader, Path)}
   */
  @Test
  public void testCreateClassLoaderWithParentPath_thenProjectBuildListenersSizeIsTwo() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(project.createClassLoader(new AntClassLoader(), Path.systemBootClasspath));
    assertEquals(2, project.getBuildListeners().size());
  }

  /**
   * Test {@link Project#createClassLoader(ClassLoader, Path)} with {@code parent}, {@code path}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(ClassLoader, Path)}
   */
  @Test
  public void testCreateClassLoaderWithParentPath_whenAntClassLoader() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(project.createClassLoader(new AntClassLoader(), Path.systemBootClasspath));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link Project#createClassLoader(ClassLoader, Path)} with {@code parent}, {@code path}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(ClassLoader, Path)}
   */
  @Test
  public void testCreateClassLoaderWithParentPath_whenNull_thenProjectBuildListenersSizeIsOne() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(project.createClassLoader(null, null));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link Project#createClassLoader(ClassLoader, Path)} with {@code parent}, {@code path}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(ClassLoader, Path)}
   */
  @Test
  public void testCreateClassLoaderWithParentPath_whenPathWithPIsProjectAndPathIsDot() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(project.createClassLoader(null, new Path(new Project(), ".")));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link Project#createClassLoader(ClassLoader, Path)} with {@code parent}, {@code path}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(ClassLoader, Path)}
   */
  @Test
  public void testCreateClassLoaderWithParentPath_whenPathWithProjectIsProject() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(project.createClassLoader(null, new Path(new Project())));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link Project#createClassLoader(Path)} with {@code path}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(Path)}
   */
  @Test
  public void testCreateClassLoaderWithPath_thenProjectBuildListenersSizeIsTwo() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(project.createClassLoader(Path.systemBootClasspath));
    assertEquals(2, project.getBuildListeners().size());
  }

  /**
   * Test {@link Project#createClassLoader(Path)} with {@code path}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(Path)}
   */
  @Test
  public void testCreateClassLoaderWithPath_whenNull_thenProjectBuildListenersSizeIsOne() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(project.createClassLoader(null));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link Project#createClassLoader(Path)} with {@code path}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code ignore}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(Path)}
   */
  @Test
  public void testCreateClassLoaderWithPath_whenPathWithPIsProjectAndPathIsIgnore() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(project.createClassLoader(new Path(new Project(), "ignore")));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link Project#createClassLoader(Path)} with {@code path}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(Path)}
   */
  @Test
  public void testCreateClassLoaderWithPath_whenPathWithProjectIsProject() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(project.createClassLoader(new Path(new Project())));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link Project#createClassLoader(Path)} with {@code path}.
   * <ul>
   *   <li>When {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#createClassLoader(Path)}
   */
  @Test
  public void testCreateClassLoaderWithPath_whenSystemBootClasspath() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(project.createClassLoader(Path.systemBootClasspath));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Project#setCoreLoader(ClassLoader)}
   *   <li>{@link Project#setDefaultInputStream(InputStream)}
   *   <li>{@link Project#setDescription(String)}
   *   <li>{@link Project#setInputHandler(InputHandler)}
   *   <li>{@link Project#setKeepGoingMode(boolean)}
   *   <li>{@link Project#inheritIDReferences(Project)}
   *   <li>{@link Project#getCoreLoader()}
   *   <li>{@link Project#getDefaultInputStream()}
   *   <li>{@link Project#getDefaultTarget()}
   *   <li>{@link Project#getGlobalFilterSet()}
   *   <li>{@link Project#getInputHandler()}
   *   <li>{@link Project#getName()}
   *   <li>{@link Project#getReferences()}
   *   <li>{@link Project#getTargets()}
   *   <li>{@link Project#isKeepGoingMode()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws IOException {
    // Arrange
    Project project = new Project();
    AntClassLoader coreLoader = new AntClassLoader();

    // Act
    project.setCoreLoader(coreLoader);
    ByteArrayInputStream defaultInputStream = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    project.setDefaultInputStream(defaultInputStream);
    project.setDescription("The characteristics of someone or something");
    DefaultInputHandler handler = new DefaultInputHandler();
    project.setInputHandler(handler);
    project.setKeepGoingMode(true);
    project.inheritIDReferences(new Project());
    ClassLoader actualCoreLoader = project.getCoreLoader();
    InputStream actualDefaultInputStream = project.getDefaultInputStream();
    String actualDefaultTarget = project.getDefaultTarget();
    FilterSet actualGlobalFilterSet = project.getGlobalFilterSet();
    InputHandler actualInputHandler = project.getInputHandler();
    String actualName = project.getName();
    project.getReferences();
    Hashtable<String, Target> actualTargets = project.getTargets();
    boolean actualIsKeepGoingModeResult = project.isKeepGoingMode();

    // Assert
    assertTrue(actualInputHandler instanceof DefaultInputHandler);
    OnMissing onMissingFiltersFile = actualGlobalFilterSet.getOnMissingFiltersFile();
    assertEquals("fail", onMissingFiltersFile.getValue());
    assertNotNull(actualCoreLoader);
    Location location = actualGlobalFilterSet.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDefaultTarget);
    assertNull(actualName);
    assertNull(actualGlobalFilterSet.getDescription());
    assertNull(actualGlobalFilterSet.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, onMissingFiltersFile.getIndex());
    assertEquals(8, actualDefaultInputStream.read(new byte[8]));
    assertFalse(actualGlobalFilterSet.isReference());
    assertFalse(actualGlobalFilterSet.hasFilters());
    assertTrue(actualTargets.isEmpty());
    assertTrue(actualIsKeepGoingModeResult);
    assertTrue(actualGlobalFilterSet.isRecurse());
    assertEquals(Project.TOKEN_END, actualGlobalFilterSet.getBeginToken());
    assertEquals(Project.TOKEN_END, actualGlobalFilterSet.getEndToken());
    assertSame(defaultInputStream, actualDefaultInputStream);
    assertSame(coreLoader, actualCoreLoader);
    assertSame(project, actualGlobalFilterSet.getProject());
    assertSame(handler, actualInputHandler);
    assertArrayEquals(new String[]{"fail", "warn", "ignore"}, onMissingFiltersFile.getValues());
  }

  /**
   * Test {@link Project#addBuildListener(BuildListener)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addBuildListener(BuildListener)}
   */
  @Test
  public void testAddBuildListener_givenProject_thenProjectBuildListenersSizeIsOne() {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();

    // Act
    project.addBuildListener(listener);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link Project#addBuildListener(BuildListener)}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addBuildListener(BuildListener)}
   */
  @Test
  public void testAddBuildListener_thenProjectBuildListenersSizeIsTwo() {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);
    AntClassLoader listener2 = new AntClassLoader();

    // Act
    project.addBuildListener(listener2);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
    assertSame(listener2, buildListeners.get(1));
  }

  /**
   * Test {@link Project#getBuildListeners()}.
   * <p>
   * Method under test: {@link Project#getBuildListeners()}
   */
  @Test
  public void testGetBuildListeners() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getBuildListeners().isEmpty());
  }

  /**
   * Test {@link Project#getProperty(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code Property Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProperty(String)}
   */
  @Test
  public void testGetProperty_givenJavaLangObject_whenPropertyName() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getProperty("Property Name"));
  }

  /**
   * Test {@link Project#getProperty(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code Property Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProperty(String)}
   */
  @Test
  public void testGetProperty_givenProjectAddBuildListenerAntClassLoader_whenPropertyName() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getProperty("Property Name"));
  }

  /**
   * Test {@link Project#getProperty(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>When {@code Property Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProperty(String)}
   */
  @Test
  public void testGetProperty_givenProjectAddTargetAddingReferenceAndTarget_whenPropertyName() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getProperty("Property Name"));
  }

  /**
   * Test {@link Project#getProperty(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProperty(String)}
   */
  @Test
  public void testGetProperty_givenProject_whenNull() {
    // Arrange, Act and Assert
    assertNull((new Project()).getProperty(null));
  }

  /**
   * Test {@link Project#getProperty(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code Property Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProperty(String)}
   */
  @Test
  public void testGetProperty_givenProject_whenPropertyName() {
    // Arrange, Act and Assert
    assertNull((new Project()).getProperty("Property Name"));
  }

  /**
   * Test {@link Project#replaceProperties(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#replaceProperties(String)}
   */
  @Test
  public void testReplaceProperties_givenJavaLangObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("42", project.replaceProperties("42"));
  }

  /**
   * Test {@link Project#replaceProperties(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#replaceProperties(String)}
   */
  @Test
  public void testReplaceProperties_givenProject() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("42", (new Project()).replaceProperties("42"));
  }

  /**
   * Test {@link Project#replaceProperties(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#replaceProperties(String)}
   */
  @Test
  public void testReplaceProperties_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("42", project.replaceProperties("42"));
  }

  /**
   * Test {@link Project#replaceProperties(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#replaceProperties(String)}
   */
  @Test
  public void testReplaceProperties_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("42", project.replaceProperties("42"));
  }

  /**
   * Test {@link Project#getUserProperty(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code Property Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getUserProperty(String)}
   */
  @Test
  public void testGetUserProperty_givenJavaLangObject_whenPropertyName() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getUserProperty("Property Name"));
  }

  /**
   * Test {@link Project#getUserProperty(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code Property Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getUserProperty(String)}
   */
  @Test
  public void testGetUserProperty_givenProjectAddBuildListenerAntClassLoader_whenPropertyName() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getUserProperty("Property Name"));
  }

  /**
   * Test {@link Project#getUserProperty(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getUserProperty(String)}
   */
  @Test
  public void testGetUserProperty_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getUserProperty("Property Name"));
  }

  /**
   * Test {@link Project#getUserProperty(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getUserProperty(String)}
   */
  @Test
  public void testGetUserProperty_givenProject_whenNull() {
    // Arrange, Act and Assert
    assertNull((new Project()).getUserProperty(null));
  }

  /**
   * Test {@link Project#getUserProperty(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code Property Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getUserProperty(String)}
   */
  @Test
  public void testGetUserProperty_givenProject_whenPropertyName() {
    // Arrange, Act and Assert
    assertNull((new Project()).getUserProperty("Property Name"));
  }

  /**
   * Test {@link Project#getProperties()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProperties()}
   */
  @Test
  public void testGetProperties_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getProperties().isEmpty());
  }

  /**
   * Test {@link Project#getProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProperties()}
   */
  @Test
  public void testGetProperties_givenProject() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getProperties().isEmpty());
  }

  /**
   * Test {@link Project#getProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProperties()}
   */
  @Test
  public void testGetProperties_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getProperties().isEmpty());
  }

  /**
   * Test {@link Project#getProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProperties()}
   */
  @Test
  public void testGetProperties_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getProperties().isEmpty());
  }

  /**
   * Test {@link Project#getPropertyNames()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link Project#getPropertyNames()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames_givenProject() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getPropertyNames().isEmpty());
  }

  /**
   * Test {@link Project#getPropertyNames()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link Project#getPropertyNames()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link Project#getUserProperties()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getUserProperties()}
   */
  @Test
  public void testGetUserProperties_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getUserProperties().isEmpty());
  }

  /**
   * Test {@link Project#getUserProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getUserProperties()}
   */
  @Test
  public void testGetUserProperties_givenProject() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getUserProperties().isEmpty());
  }

  /**
   * Test {@link Project#getUserProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getUserProperties()}
   */
  @Test
  public void testGetUserProperties_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getUserProperties().isEmpty());
  }

  /**
   * Test {@link Project#getUserProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getUserProperties()}
   */
  @Test
  public void testGetUserProperties_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getUserProperties().isEmpty());
  }

  /**
   * Test {@link Project#getInheritedProperties()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getInheritedProperties()}
   */
  @Test
  public void testGetInheritedProperties_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getInheritedProperties().isEmpty());
  }

  /**
   * Test {@link Project#getInheritedProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getInheritedProperties()}
   */
  @Test
  public void testGetInheritedProperties_givenProject() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getInheritedProperties().isEmpty());
  }

  /**
   * Test {@link Project#getInheritedProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getInheritedProperties()}
   */
  @Test
  public void testGetInheritedProperties_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getInheritedProperties().isEmpty());
  }

  /**
   * Test {@link Project#getInheritedProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getInheritedProperties()}
   */
  @Test
  public void testGetInheritedProperties_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getInheritedProperties().isEmpty());
  }

  /**
   * Test {@link Project#getDescription()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDescription()}
   */
  @Test
  public void testGetDescription_givenJavaLangObject_thenReturnNull() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getDescription());
  }

  /**
   * Test {@link Project#getDescription()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDescription()}
   */
  @Test
  public void testGetDescription_givenProjectAddBuildListenerAntClassLoader_thenReturnNull() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getDescription());
  }

  /**
   * Test {@link Project#getDescription()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@link MagicNames#REFID_PROPERTY_HELPER} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDescription()}
   */
  @Test
  public void testGetDescription_givenProjectAddReferenceRefid_property_helperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getDescription());
  }

  /**
   * Test {@link Project#getDescription()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@link MagicNames#REFID_PROPERTY_HELPER} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDescription()}
   */
  @Test
  public void testGetDescription_givenProjectAddTargetRefid_property_helperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget(MagicNames.REFID_PROPERTY_HELPER, new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getDescription());
  }

  /**
   * Test {@link Project#getDescription()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDescription()}
   */
  @Test
  public void testGetDescription_givenProject_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Project()).getDescription());
  }

  /**
   * Test {@link Project#getDescription()}.
   * <ul>
   *   <li>Then return {@code The characteristics of someone or something}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDescription()}
   */
  @Test
  public void testGetDescription_thenReturnTheCharacteristicsOfSomeoneOrSomething() {
    // Arrange
    Project project = new Project();
    project.setDescription("The characteristics of someone or something");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("The characteristics of someone or something", project.getDescription());
  }

  /**
   * Test {@link Project#addFilter(String, String)}.
   * <ul>
   *   <li>When {@code ABC123}.</li>
   *   <li>Then {@link Project} (default constructor) Filters size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addFilter(String, String)}
   */
  @Test
  public void testAddFilter_whenAbc123_thenProjectFiltersSizeIsOne() {
    // Arrange
    Project project = new Project();

    // Act
    project.addFilter("ABC123", "42");

    // Assert
    Hashtable<String, String> filters = project.getFilters();
    assertEquals(1, filters.size());
    assertEquals("42", filters.get("ABC123"));
    assertTrue(project.getGlobalFilterSet().hasFilters());
  }

  /**
   * Test {@link Project#addFilter(String, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then not {@link Project} (default constructor) GlobalFilterSet hasFilters.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addFilter(String, String)}
   */
  @Test
  public void testAddFilter_whenNull_thenNotProjectGlobalFilterSetHasFilters() {
    // Arrange
    Project project = new Project();

    // Act
    project.addFilter(null, "42");

    // Assert that nothing has changed
    assertFalse(project.getGlobalFilterSet().hasFilters());
    assertTrue(project.getFilters().isEmpty());
  }

  /**
   * Test {@link Project#getFilters()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addFilter {@code ABC123} and {@code 42}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getFilters()}
   */
  @Test
  public void testGetFilters_givenProjectAddFilterAbc123And42_thenReturnSizeIsOne() {
    // Arrange
    Project project = new Project();
    project.addFilter("ABC123", "42");

    // Act
    Hashtable<String, String> actualFilters = project.getFilters();

    // Assert
    assertEquals(1, actualFilters.size());
    assertEquals("42", actualFilters.get("ABC123"));
  }

  /**
   * Test {@link Project#getFilters()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addFilter {@code Token} and {@code Value}.</li>
   *   <li>Then return size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getFilters()}
   */
  @Test
  public void testGetFilters_givenProjectAddFilterTokenAndValue_thenReturnSizeIsTwo() {
    // Arrange
    Project project = new Project();
    project.addFilter("Token", "Value");
    project.addFilter("ABC123", "42");

    // Act
    Hashtable<String, String> actualFilters = project.getFilters();

    // Assert
    assertEquals(2, actualFilters.size());
    assertEquals("42", actualFilters.get("ABC123"));
    assertEquals("Value", actualFilters.get("Token"));
  }

  /**
   * Test {@link Project#getFilters()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getFilters()}
   */
  @Test
  public void testGetFilters_givenProject_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getFilters().isEmpty());
  }

  /**
   * Test {@link Project#setBasedir(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then {@link Project} (default constructor) BaseDir Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setBasedir(String)}
   */
  @Test
  public void testSetBasedir_givenJavaLangObject_thenProjectBaseDirNameIsApacheAnt11015() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Users", typeClass);

    // Act
    project.setBasedir(".");

    // Assert that nothing has changed
    assertEquals("apache-ant-1.10.15", project.getBaseDir().getName());
    Hashtable<String, Object> properties = project.getProperties();
    assertEquals(1, properties.size());
    String expectedString = System.getProperty("user.dir");
    assertEquals(expectedString, properties.get(MagicNames.PROJECT_BASEDIR));
  }

  /**
   * Test {@link Project#setBasedir(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setBasedir(String)}
   */
  @Test
  public void testSetBasedir_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    project.setBasedir(".");

    // Assert that nothing has changed
    assertEquals("apache-ant-1.10.15", project.getBaseDir().getName());
    Hashtable<String, Object> properties = project.getProperties();
    assertEquals(1, properties.size());
    String expectedString = System.getProperty("user.dir");
    assertEquals(expectedString, properties.get(MagicNames.PROJECT_BASEDIR));
  }

  /**
   * Test {@link Project#setBasedir(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setBasedir(String)}
   */
  @Test
  public void testSetBasedir_givenProjectAddBuildListenerDefaultLogger() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    // Act
    project.setBasedir(".");

    // Assert that nothing has changed
    assertEquals("apache-ant-1.10.15", project.getBaseDir().getName());
    Hashtable<String, Object> properties = project.getProperties();
    assertEquals(1, properties.size());
    String expectedString = System.getProperty("user.dir");
    assertEquals(expectedString, properties.get(MagicNames.PROJECT_BASEDIR));
  }

  /**
   * Test {@link Project#setBasedir(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link NoBannerLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setBasedir(String)}
   */
  @Test
  public void testSetBasedir_givenProjectAddBuildListenerNoBannerLogger() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new NoBannerLogger());

    // Act
    project.setBasedir(".");

    // Assert that nothing has changed
    assertEquals("apache-ant-1.10.15", project.getBaseDir().getName());
    Hashtable<String, Object> properties = project.getProperties();
    assertEquals(1, properties.size());
    String expectedString = System.getProperty("user.dir");
    assertEquals(expectedString, properties.get(MagicNames.PROJECT_BASEDIR));
  }

  /**
   * Test {@link Project#setBasedir(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code Adding reference:}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setBasedir(String)}
   */
  @Test
  public void testSetBasedir_givenProject_whenAddingReference_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Project()).setBasedir("Adding reference: "));
  }

  /**
   * Test {@link Project#setBasedir(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code Base D}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setBasedir(String)}
   */
  @Test
  public void testSetBasedir_givenProject_whenBaseD_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Project()).setBasedir("Base D"));
  }

  /**
   * Test {@link Project#setBasedir(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code ..}.</li>
   *   <li>Then {@link Project} (default constructor) BaseDir Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setBasedir(String)}
   */
  @Test
  public void testSetBasedir_givenProject_whenDotDot_thenProjectBaseDirNameIsDownloads() throws BuildException {
    // Arrange
    Project project = new Project();

    // Act
    project.setBasedir("..");

    // Assert
    assertEquals("Downloads", project.getBaseDir().getName());
    Hashtable<String, Object> properties = project.getProperties();
    assertEquals(1, properties.size());
    String expectedString = Paths.get(System.getProperty("user.home"), "Downloads").toString();
    assertEquals(expectedString, properties.get(MagicNames.PROJECT_BASEDIR));
  }

  /**
   * Test {@link Project#setBasedir(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code .}.</li>
   *   <li>Then {@link Project} (default constructor) BaseDir Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setBasedir(String)}
   */
  @Test
  public void testSetBasedir_givenProject_whenDot_thenProjectBaseDirNameIsApacheAnt11015() throws BuildException {
    // Arrange
    Project project = new Project();

    // Act
    project.setBasedir(".");

    // Assert that nothing has changed
    assertEquals("apache-ant-1.10.15", project.getBaseDir().getName());
    Hashtable<String, Object> properties = project.getProperties();
    assertEquals(1, properties.size());
    String expectedString = System.getProperty("user.dir");
    assertEquals(expectedString, properties.get(MagicNames.PROJECT_BASEDIR));
  }

  /**
   * Test {@link Project#setBaseDir(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code var} toFile.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setBaseDir(File)}
   */
  @Test
  public void testSetBaseDir_whenPropertyIsJavaIoTmpdirIsVarToFile_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "var").toFile()));
  }

  /**
   * Test {@link Project#getBaseDir()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getBaseDir()}
   */
  @Test
  public void testGetBaseDir_givenJavaLangObject_thenReturnNameIsApacheAnt11015() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Users", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act
    File actualBaseDir = project.getBaseDir();

    // Assert
    assertEquals("apache-ant-1.10.15", actualBaseDir.getName());
    assertTrue(actualBaseDir.isAbsolute());
  }

  /**
   * Test {@link Project#getBaseDir()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getBaseDir()}
   */
  @Test
  public void testGetBaseDir_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    File actualBaseDir = project.getBaseDir();

    // Assert
    assertEquals("apache-ant-1.10.15", actualBaseDir.getName());
    assertTrue(actualBaseDir.isAbsolute());
  }

  /**
   * Test {@link Project#getBaseDir()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getBaseDir()}
   */
  @Test
  public void testGetBaseDir_givenProject_thenReturnNameIsApacheAnt11015() {
    // Arrange and Act
    File actualBaseDir = (new Project()).getBaseDir();

    // Assert
    assertEquals("apache-ant-1.10.15", actualBaseDir.getName());
    assertTrue(actualBaseDir.isAbsolute());
  }

  /**
   * Test {@link Project#getJavaVersion()}.
   * <p>
   * Method under test: {@link Project#getJavaVersion()}
   */
  @Test
  public void testGetJavaVersion() {
    // Arrange, Act and Assert
    assertEquals("17", Project.getJavaVersion());
  }

  /**
   * Test {@link Project#getTaskDefinitions()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getTaskDefinitions()}
   */
  @Test
  public void testGetTaskDefinitions_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getTaskDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getTaskDefinitions()}
   */
  @Test
  public void testGetTaskDefinitions_givenProject() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getTaskDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getTaskDefinitions()}
   */
  @Test
  public void testGetTaskDefinitions_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getTaskDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@link MagicNames#REFID_PROPERTY_HELPER} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getTaskDefinitions()}
   */
  @Test
  public void testGetTaskDefinitions_givenProjectAddReferenceRefid_property_helperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getTaskDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getTaskDefinitions()}
   */
  @Test
  public void testGetTaskDefinitions_givenProjectDefaultIsComponent_helper_reference() {
    // Arrange
    Project project = new Project();
    project.setDefault(ComponentHelper.COMPONENT_HELPER_REFERENCE);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getCopyOfTaskDefinitions()}.
   * <p>
   * Method under test: {@link Project#getCopyOfTaskDefinitions()}
   */
  @Test
  public void testGetCopyOfTaskDefinitions() {
    // Arrange
    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getCopyOfTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getCopyOfTaskDefinitions()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getCopyOfTaskDefinitions()}
   */
  @Test
  public void testGetCopyOfTaskDefinitions_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getCopyOfTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getCopyOfTaskDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getCopyOfTaskDefinitions()}
   */
  @Test
  public void testGetCopyOfTaskDefinitions_givenProject() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getCopyOfTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getCopyOfTaskDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getCopyOfTaskDefinitions()}
   */
  @Test
  public void testGetCopyOfTaskDefinitions_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getCopyOfTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getCopyOfTaskDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getCopyOfTaskDefinitions()}
   */
  @Test
  public void testGetCopyOfTaskDefinitions_givenProjectDefaultIsComponent_helper_reference() {
    // Arrange
    Project project = new Project();
    project.setDefault(ComponentHelper.COMPONENT_HELPER_REFERENCE);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getCopyOfTaskDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#addDataTypeDefinition(String, Class)}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) CopyOfReferences size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addDataTypeDefinition(String, Class)}
   */
  @Test
  public void testAddDataTypeDefinition_thenProjectCopyOfReferencesSizeIsTwo() {
    // Arrange
    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());
    Class<Object> typeClass = Object.class;

    // Act
    project.addDataTypeDefinition("Type Name", typeClass);

    // Assert
    Map<String, Object> copyOfReferences = project.getCopyOfReferences();
    assertEquals(2, copyOfReferences.size());
    Object getResult = copyOfReferences.get(ComponentHelper.COMPONENT_HELPER_REFERENCE);
    assertTrue(getResult instanceof ComponentHelper);
    Hashtable<String, AntTypeDefinition> antTypeTable = ((ComponentHelper) getResult).getAntTypeTable();
    assertEquals(1, antTypeTable.size());
    Hashtable<String, Class<?>> dataTypeDefinitions = project.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertTrue(antTypeTable.containsKey("Type Name"));
    assertTrue(dataTypeDefinitions.containsKey("Type Name"));
    assertTrue(project.getFilters().isEmpty());
    assertTrue(project.getTargets().isEmpty());
    assertTrue(project.getTaskDefinitions().isEmpty());
    assertTrue(copyOfReferences.containsKey(MagicNames.REFID_PROPERTY_HELPER));
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link Project#getDataTypeDefinitions()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDataTypeDefinitions()}
   */
  @Test
  public void testGetDataTypeDefinitions_givenJavaLangObject_thenReturnSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act
    Hashtable<String, Class<?>> actualDataTypeDefinitions = project.getDataTypeDefinitions();

    // Assert
    assertEquals(1, actualDataTypeDefinitions.size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, actualDataTypeDefinitions.get(MagicNames.REFID_PROPERTY_HELPER));
  }

  /**
   * Test {@link Project#getDataTypeDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@link MagicNames#REFID_PROPERTY_HELPER} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDataTypeDefinitions()}
   */
  @Test
  public void testGetDataTypeDefinitions_givenProjectAddReferenceRefid_property_helperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getDataTypeDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDataTypeDefinitions()}
   */
  @Test
  public void testGetDataTypeDefinitions_givenProjectDefaultIsComponent_helper_reference() {
    // Arrange
    Project project = new Project();
    project.setDefault(ComponentHelper.COMPONENT_HELPER_REFERENCE);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getDataTypeDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDataTypeDefinitions()}
   */
  @Test
  public void testGetDataTypeDefinitions_givenProject_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getDataTypeDefinitions()}.
   * <ul>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getDataTypeDefinitions()}
   */
  @Test
  public void testGetDataTypeDefinitions_thenReturnEmpty() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getCopyOfDataTypeDefinitions()}.
   * <p>
   * Method under test: {@link Project#getCopyOfDataTypeDefinitions()}
   */
  @Test
  public void testGetCopyOfDataTypeDefinitions() {
    // Arrange
    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getCopyOfDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getCopyOfDataTypeDefinitions()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getCopyOfDataTypeDefinitions()}
   */
  @Test
  public void testGetCopyOfDataTypeDefinitions_givenJavaLangObject_thenReturnSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act
    Map<String, Class<?>> actualCopyOfDataTypeDefinitions = project.getCopyOfDataTypeDefinitions();

    // Assert
    assertEquals(1, actualCopyOfDataTypeDefinitions.size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, actualCopyOfDataTypeDefinitions.get(MagicNames.REFID_PROPERTY_HELPER));
  }

  /**
   * Test {@link Project#getCopyOfDataTypeDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getCopyOfDataTypeDefinitions()}
   */
  @Test
  public void testGetCopyOfDataTypeDefinitions_givenProjectDefaultIsComponent_helper_reference() {
    // Arrange
    Project project = new Project();
    project.setDefault(ComponentHelper.COMPONENT_HELPER_REFERENCE);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getCopyOfDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getCopyOfDataTypeDefinitions()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getCopyOfDataTypeDefinitions()}
   */
  @Test
  public void testGetCopyOfDataTypeDefinitions_givenProject_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getCopyOfDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#getCopyOfDataTypeDefinitions()}.
   * <ul>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getCopyOfDataTypeDefinitions()}
   */
  @Test
  public void testGetCopyOfDataTypeDefinitions_thenReturnEmpty() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.getCopyOfDataTypeDefinitions().isEmpty());
  }

  /**
   * Test {@link Project#addTarget(String, Target)} with {@code targetName}, {@code target}.
   * <p>
   * Method under test: {@link Project#addTarget(String, Target)}
   */
  @Test
  public void testAddTargetWithTargetNameTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);
    Target target = new Target();

    // Act
    project.addTarget("Target Name", target);

    // Assert
    Project project2 = target.getProject();
    Vector<BuildListener> buildListeners = project2.getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link Project#addTarget(String, Target)} with {@code targetName}, {@code target}.
   * <ul>
   *   <li>Then {@link Target#Target()} Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addTarget(String, Target)}
   */
  @Test
  public void testAddTargetWithTargetNameTarget_thenTargetProjectBuildListenersEmpty() throws BuildException {
    // Arrange
    Project project = new Project();
    Target target = new Target();

    // Act
    project.addTarget("Target Name", target);

    // Assert
    Project project2 = target.getProject();
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project2.getBuildListeners().isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
  }

  /**
   * Test {@link Project#addTarget(String, Target)} with {@code targetName}, {@code target}.
   * <ul>
   *   <li>Then {@link Target#Target()} Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addTarget(String, Target)}
   */
  @Test
  public void testAddTargetWithTargetNameTarget_thenTargetProjectDataTypeDefinitionsSizeIsOne() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());
    Target target = new Target();

    // Act
    project.addTarget("Target Name", target);

    // Assert
    Project project2 = target.getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertEquals(1, project2.getBuildListeners().size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get("Type Name"));
    assertSame(typeClass, dataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link Project#addTarget(String, Target)} with {@code targetName}, {@code target}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addTarget(String, Target)}
   */
  @Test
  public void testAddTargetWithTargetNameTarget_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Target Name", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> project.addTarget("Target Name", new Target()));
  }

  /**
   * Test {@link Project#addTarget(Target)} with {@code target}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then {@link Target#Target()} Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addTarget(Target)}
   */
  @Test
  public void testAddTargetWithTarget_givenProject_thenTargetProjectBuildListenersEmpty() throws BuildException {
    // Arrange
    Project project = new Project();

    Target target = new Target();
    target.setName("Name");

    // Act
    project.addTarget(target);

    // Assert
    Project project2 = target.getProject();
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project2.getBuildListeners().isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
  }

  /**
   * Test {@link Project#addTarget(Target)} with {@code target}.
   * <ul>
   *   <li>Then {@link Target#Target()} Project BuildListeners first is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addTarget(Target)}
   */
  @Test
  public void testAddTargetWithTarget_thenTargetProjectBuildListenersFirstIsAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    Target target = new Target();
    target.setName("Name");

    // Act
    project.addTarget(target);

    // Assert
    Vector<BuildListener> buildListeners = target.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link Project#addTarget(Target)} with {@code target}.
   * <ul>
   *   <li>Then {@link Target#Target()} Project BuildListeners first is {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addTarget(Target)}
   */
  @Test
  public void testAddTargetWithTarget_thenTargetProjectBuildListenersFirstIsDefaultLogger() throws BuildException {
    // Arrange
    Project project = new Project();
    DefaultLogger listener = new DefaultLogger();
    project.addBuildListener(listener);

    Target target = new Target();
    target.setName("Name");

    // Act
    project.addTarget(target);

    // Assert
    Vector<BuildListener> buildListeners = target.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link Project#addTarget(Target)} with {@code target}.
   * <ul>
   *   <li>Then {@link Target#Target()} Project BuildListeners first is {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addTarget(Target)}
   */
  @Test
  public void testAddTargetWithTarget_thenTargetProjectBuildListenersFirstIsExecutorTest() throws BuildException {
    // Arrange
    Project project = new Project();
    ExecutorTest listener = new ExecutorTest();
    project.addBuildListener(listener);

    Target target = new Target();
    target.setName("Name");

    // Act
    project.addTarget(target);

    // Assert
    Vector<BuildListener> buildListeners = target.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link Project#addTarget(Target)} with {@code target}.
   * <ul>
   *   <li>Then {@link Target#Target()} Project BuildListeners first {@link MockBuildListener}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addTarget(Target)}
   */
  @Test
  public void testAddTargetWithTarget_thenTargetProjectBuildListenersFirstMockBuildListener() throws BuildException {
    // Arrange
    Project project = new Project();
    MockBuildListener listener = new MockBuildListener(new Project());
    project.addBuildListener(listener);

    Target target = new Target();
    target.setName("Name");

    // Act
    project.addTarget(target);

    // Assert
    Project project2 = target.getProject();
    Vector<BuildListener> buildListeners = project2.getBuildListeners();
    assertEquals(1, buildListeners.size());
    BuildListener getResult = buildListeners.get(0);
    assertTrue(getResult instanceof MockBuildListener);
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertSame(listener, getResult);
  }

  /**
   * Test {@link Project#addTarget(Target)} with {@code target}.
   * <ul>
   *   <li>Then {@link Target#Target()} Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addTarget(Target)}
   */
  @Test
  public void testAddTargetWithTarget_thenTargetProjectDataTypeDefinitionsSizeIsOne() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("\".", typeClass);

    Target target = new Target();
    target.setName("Name");

    // Act
    project.addTarget(target);

    // Assert
    Project project2 = target.getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get("\"."));
    assertSame(typeClass, dataTypeDefinitions.get("\"."));
  }

  /**
   * Test {@link Project#addOrReplaceTarget(String, Target)} with {@code targetName}, {@code target}.
   * <p>
   * Method under test: {@link Project#addOrReplaceTarget(String, Target)}
   */
  @Test
  public void testAddOrReplaceTargetWithTargetNameTarget() {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);
    Target target = new Target();

    // Act
    project.addOrReplaceTarget("Target Name", target);

    // Assert
    Project project2 = target.getProject();
    Vector<BuildListener> buildListeners = project2.getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link Project#addOrReplaceTarget(String, Target)} with {@code targetName}, {@code target}.
   * <p>
   * Method under test: {@link Project#addOrReplaceTarget(String, Target)}
   */
  @Test
  public void testAddOrReplaceTargetWithTargetNameTarget2() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());
    Target target = new Target();

    // Act
    project.addOrReplaceTarget("Target Name", target);

    // Assert
    Project project2 = target.getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertEquals(1, project2.getBuildListeners().size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get("Type Name"));
    assertSame(typeClass, dataTypeDefinitions.get("Type Name"));
  }

  /**
   * Test {@link Project#addOrReplaceTarget(String, Target)} with {@code targetName}, {@code target}.
   * <p>
   * Method under test: {@link Project#addOrReplaceTarget(String, Target)}
   */
  @Test
  public void testAddOrReplaceTargetWithTargetNameTarget3() {
    // Arrange
    Project project = new Project();
    DefaultLogger listener = new DefaultLogger();
    project.addBuildListener(listener);
    Target target = new Target();

    // Act
    project.addOrReplaceTarget("Target Name", target);

    // Assert
    Vector<BuildListener> buildListeners = target.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link Project#addOrReplaceTarget(String, Target)} with {@code targetName}, {@code target}.
   * <p>
   * Method under test: {@link Project#addOrReplaceTarget(String, Target)}
   */
  @Test
  public void testAddOrReplaceTargetWithTargetNameTarget4() {
    // Arrange
    Project project = new Project();
    ExecutorTest listener = new ExecutorTest();
    project.addBuildListener(listener);
    Target target = new Target();

    // Act
    project.addOrReplaceTarget("Target Name", target);

    // Assert
    Vector<BuildListener> buildListeners = target.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link Project#addOrReplaceTarget(String, Target)} with {@code targetName}, {@code target}.
   * <p>
   * Method under test: {@link Project#addOrReplaceTarget(String, Target)}
   */
  @Test
  public void testAddOrReplaceTargetWithTargetNameTarget5() {
    // Arrange
    Project project = new Project();
    MockBuildListener listener = new MockBuildListener(new Project());
    project.addBuildListener(listener);
    Target target = new Target();

    // Act
    project.addOrReplaceTarget("Target Name", target);

    // Assert
    Project project2 = target.getProject();
    Vector<BuildListener> buildListeners = project2.getBuildListeners();
    assertEquals(1, buildListeners.size());
    BuildListener getResult = buildListeners.get(0);
    assertTrue(getResult instanceof MockBuildListener);
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertSame(listener, getResult);
  }

  /**
   * Test {@link Project#addOrReplaceTarget(String, Target)} with {@code targetName}, {@code target}.
   * <ul>
   *   <li>Then {@link Target#Target()} Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addOrReplaceTarget(String, Target)}
   */
  @Test
  public void testAddOrReplaceTargetWithTargetNameTarget_thenTargetProjectBuildListenersEmpty() {
    // Arrange
    Project project = new Project();
    Target target = new Target();

    // Act
    project.addOrReplaceTarget("Target Name", target);

    // Assert
    Project project2 = target.getProject();
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project2.getBuildListeners().isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
  }

  /**
   * Test {@link Project#addOrReplaceTarget(Target)} with {@code target}.
   * <ul>
   *   <li>Given {@code +Target:}.</li>
   *   <li>Then {@link Target#Target()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#addOrReplaceTarget(Target)}
   */
  @Test
  public void testAddOrReplaceTargetWithTarget_givenTarget_thenTargetProjectIsProject() {
    // Arrange
    Project project = new Project();

    Target target = new Target();
    target.setName(" +Target: ");

    // Act
    project.addOrReplaceTarget(target);

    // Assert
    assertSame(project, target.getProject());
  }

  /**
   * Test {@link Project#getCopyOfTargets()}.
   * <p>
   * Method under test: {@link Project#getCopyOfTargets()}
   */
  @Test
  public void testGetCopyOfTargets() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getCopyOfTargets().isEmpty());
  }

  /**
   * Test {@link Project#setExecutor(Executor)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#setExecutor(Executor)}
   */
  @Test
  public void testSetExecutor_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());
    DefaultExecutor e = new DefaultExecutor();

    // Act
    project.setExecutor(e);

    // Assert
    Hashtable<String, Object> references = project.getReferences();
    assertEquals(3, references.size());
    Map<String, Object> copyOfReferences = project.getCopyOfReferences();
    assertEquals(3, copyOfReferences.size());
    assertTrue(references.containsKey(ComponentHelper.COMPONENT_HELPER_REFERENCE));
    assertTrue(references.containsKey(MagicNames.REFID_PROPERTY_HELPER));
    assertTrue(copyOfReferences.containsKey(ComponentHelper.COMPONENT_HELPER_REFERENCE));
    assertTrue(copyOfReferences.containsKey(MagicNames.REFID_PROPERTY_HELPER));
    assertSame(e, references.get(MagicNames.ANT_EXECUTOR_REFERENCE));
    assertSame(e, copyOfReferences.get(MagicNames.ANT_EXECUTOR_REFERENCE));
    assertSame(e, project.getExecutor());
  }

  /**
   * Test {@link Project#getExecutor()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getExecutor()}
   */
  @Test
  public void testGetExecutor_givenProject() {
    // Arrange and Act
    Executor actualExecutor = (new Project()).getExecutor();

    // Assert
    assertTrue(actualExecutor instanceof DefaultExecutor);
    assertTrue(actualExecutor.getSubProjectExecutor() instanceof SingleCheckExecutor);
  }

  /**
   * Test {@link Project#getExecutor()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getExecutor()}
   */
  @Test
  public void testGetExecutor_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    Executor actualExecutor = project.getExecutor();

    // Assert
    assertTrue(actualExecutor instanceof DefaultExecutor);
    assertTrue(actualExecutor.getSubProjectExecutor() instanceof SingleCheckExecutor);
  }

  /**
   * Test {@link Project#getExecutor()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getExecutor()}
   */
  @Test
  public void testGetExecutor_givenProjectCoreLoaderIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    project.addBuildListener(new AntClassLoader());

    // Act
    Executor actualExecutor = project.getExecutor();

    // Assert
    assertTrue(actualExecutor instanceof DefaultExecutor);
    assertTrue(actualExecutor.getSubProjectExecutor() instanceof SingleCheckExecutor);
  }

  /**
   * Test {@link Project#executeTargets(Vector)}.
   * <p>
   * Method under test: {@link Project#executeTargets(Vector)}
   */
  @Test
  public void testExecuteTargets() throws BuildException {
    // Arrange
    Project project = new Project();
    AntClassLoader parent = new AntClassLoader();
    project.setCoreLoader(AntClassLoader.newAntClassLoader(parent, new Project(), Path.systemBootClasspath, true));
    project.addBuildListener(new AntClassLoader());
    Vector<String> names = Execute.getProcEnvironment();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.executeTargets(names));
  }

  /**
   * Test {@link Project#executeTargets(Vector)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#executeTargets(Vector)}
   */
  @Test
  public void testExecuteTargets_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    Vector<String> names = Execute.getProcEnvironment();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.executeTargets(names));
  }

  /**
   * Test {@link Project#executeTargets(Vector)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#executeTargets(Vector)}
   */
  @Test
  public void testExecuteTargets_givenProjectAddBuildListenerDefaultLogger() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());
    Vector<String> names = Execute.getProcEnvironment();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.executeTargets(names));
  }

  /**
   * Test {@link Project#executeTargets(Vector)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link NoBannerLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#executeTargets(Vector)}
   */
  @Test
  public void testExecuteTargets_givenProjectAddBuildListenerNoBannerLogger() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new NoBannerLogger());
    Vector<String> names = Execute.getProcEnvironment();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.executeTargets(names));
  }

  /**
   * Test {@link Project#executeTargets(Vector)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#executeTargets(Vector)}
   */
  @Test
  public void testExecuteTargets_givenProjectCoreLoaderIsAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    project.addBuildListener(new AntClassLoader());
    Vector<String> names = Execute.getProcEnvironment();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.executeTargets(names));
  }

  /**
   * Test {@link Project#executeTargets(Vector)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) KeepGoingMode is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#executeTargets(Vector)}
   */
  @Test
  public void testExecuteTargets_givenProjectKeepGoingModeIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setKeepGoingMode(true);
    project.addBuildListener(new AntClassLoader());
    Vector<String> names = Execute.getProcEnvironment();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.executeTargets(names));
  }

  /**
   * Test {@link Project#executeTargets(Vector)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When ProcEnvironment.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#executeTargets(Vector)}
   */
  @Test
  public void testExecuteTargets_givenProject_whenProcEnvironment_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Vector<String> names = Execute.getProcEnvironment();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.executeTargets(names));
  }

  /**
   * Test {@link Project#defaultInput(byte[], int, int)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link EOFException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#defaultInput(byte[], int, int)}
   */
  @Test
  public void testDefaultInput_givenProject_whenAxaxaxaxBytesIsUtf8_thenThrowEOFException() throws IOException {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertThrows(EOFException.class, () -> project.defaultInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link Project#defaultInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#defaultInput(byte[], int, int)}
   */
  @Test
  public void testDefaultInput_thenReturnThree() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Act and Assert
    assertEquals(3, project.defaultInput(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, project.getDefaultInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Project#demuxInput(byte[], int, int)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link EOFException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#demuxInput(byte[], int, int)}
   */
  @Test
  public void testDemuxInput_givenProject_whenAxaxaxaxBytesIsUtf8_thenThrowEOFException() throws IOException {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertThrows(EOFException.class, () -> project.demuxInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link Project#demuxInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#demuxInput(byte[], int, int)}
   */
  @Test
  public void testDemuxInput_thenReturnThree() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Act and Assert
    assertEquals(3, project.demuxInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, project.getDefaultInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Project#executeTarget(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) KeepGoingMode is {@code false}.</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#executeTarget(String)}
   */
  @Test
  public void testExecuteTarget_givenProjectKeepGoingModeIsFalse_whenNull() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setKeepGoingMode(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> project.executeTarget(null));
  }

  /**
   * Test {@link Project#executeTarget(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code Target Name}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#executeTarget(String)}
   */
  @Test
  public void testExecuteTarget_givenProject_whenTargetName_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Project()).executeTarget("Target Name"));
  }

  /**
   * Test {@link Project#resolveFile(String, File)} with {@code fileName}, {@code rootDir}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then return Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#resolveFile(String, File)}
   */
  @Test
  public void testResolveFileWithFileNameRootDir_whenDot_thenReturnNameIsTestTxt() {
    // Arrange
    Project project = new Project();

    // Act
    File actualResolveFileResult = project.resolveFile(".",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals("test.txt", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link Project#resolveFile(String, File)} with {@code fileName}, {@code rootDir}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#resolveFile(String, File)}
   */
  @Test
  public void testResolveFileWithFileNameRootDir_whenEmptyString_thenReturnNameIsTestTxt() {
    // Arrange
    Project project = new Project();

    // Act
    File actualResolveFileResult = project.resolveFile("",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals("test.txt", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link Project#resolveFile(String, File)} with {@code fileName}, {@code rootDir}.
   * <ul>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return Name is {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#resolveFile(String, File)}
   */
  @Test
  public void testResolveFileWithFileNameRootDir_whenFooTxt_thenReturnNameIsFooTxt() {
    // Arrange
    Project project = new Project();

    // Act
    File actualResolveFileResult = project.resolveFile("foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals("foo.txt", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link Project#resolveFile(String)} with {@code fileName}.
   * <ul>
   *   <li>When {@code ..}.</li>
   *   <li>Then return Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#resolveFile(String)}
   */
  @Test
  public void testResolveFileWithFileName_whenDotDot_thenReturnNameIsDownloads() {
    // Arrange and Act
    File actualResolveFileResult = (new Project()).resolveFile("..");

    // Assert
    assertEquals("Downloads", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link Project#resolveFile(String)} with {@code fileName}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#resolveFile(String)}
   */
  @Test
  public void testResolveFileWithFileName_whenDot_thenReturnNameIsApacheAnt11015() {
    // Arrange and Act
    File actualResolveFileResult = (new Project()).resolveFile(".");

    // Assert
    assertEquals("apache-ant-1.10.15", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link Project#resolveFile(String)} with {@code fileName}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#resolveFile(String)}
   */
  @Test
  public void testResolveFileWithFileName_whenEmptyString_thenReturnNameIsApacheAnt11015() {
    // Arrange and Act
    File actualResolveFileResult = (new Project()).resolveFile("");

    // Assert
    assertEquals("apache-ant-1.10.15", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link Project#resolveFile(String)} with {@code fileName}.
   * <ul>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return Name is {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#resolveFile(String)}
   */
  @Test
  public void testResolveFileWithFileName_whenFooTxt_thenReturnNameIsFooTxt() {
    // Arrange and Act
    File actualResolveFileResult = (new Project()).resolveFile("foo.txt");

    // Assert
    assertEquals("foo.txt", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link Project#translatePath(String)}.
   * <ul>
   *   <li>When {@code '}.</li>
   *   <li>Then return {@code '}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#translatePath(String)}
   */
  @Test
  public void testTranslatePath_whenApostrophe_thenReturnApostrophe() {
    // Arrange, Act and Assert
    assertEquals("'", Project.translatePath("'"));
  }

  /**
   * Test {@link Project#translatePath(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#translatePath(String)}
   */
  @Test
  public void testTranslatePath_whenEmptyString_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Project.translatePath(""));
  }

  /**
   * Test {@link Project#translatePath(String)}.
   * <ul>
   *   <li>When {@code To Process}.</li>
   *   <li>Then return {@code To Process}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#translatePath(String)}
   */
  @Test
  public void testTranslatePath_whenToProcess_thenReturnToProcess() {
    // Arrange, Act and Assert
    assertEquals("To Process", Project.translatePath("To Process"));
  }

  /**
   * Test {@link Project#toBoolean(String)}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#toBoolean(String)}
   */
  @Test
  public void testToBoolean_whenFoo_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Project.toBoolean("foo"));
  }

  /**
   * Test {@link Project#toBoolean(String)}.
   * <ul>
   *   <li>When {@code on}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#toBoolean(String)}
   */
  @Test
  public void testToBoolean_whenOn_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Project.toBoolean("on"));
  }

  /**
   * Test {@link Project#toBoolean(String)}.
   * <ul>
   *   <li>When {@link Boolean#TRUE} toString.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#toBoolean(String)}
   */
  @Test
  public void testToBoolean_whenTrueToString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Project.toBoolean(Boolean.TRUE.toString()));
  }

  /**
   * Test {@link Project#toBoolean(String)}.
   * <ul>
   *   <li>When {@code yes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#toBoolean(String)}
   */
  @Test
  public void testToBoolean_whenYes_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Project.toBoolean("yes"));
  }

  /**
   * Test {@link Project#getProject(Object)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProject(Object)}
   */
  @Test
  public void testGetProject_when42() {
    // Arrange, Act and Assert
    assertNull(Project.getProject("42"));
  }

  /**
   * Test {@link Project#getProject(Object)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProject(Object)}
   */
  @Test
  public void testGetProject_whenNull() {
    // Arrange, Act and Assert
    assertNull(Project.getProject(null));
  }

  /**
   * Test {@link Project#getProject(Object)}.
   * <ul>
   *   <li>When {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getProject(Object)}
   */
  @Test
  public void testGetProject_whenSystemBootClasspath() {
    // Arrange, Act and Assert
    assertNull(Project.getProject(Path.systemBootClasspath));
  }

  /**
   * Test {@link Project#topoSort(String, Hashtable, boolean)} with {@code root}, {@code targetTable}, {@code returnAll}.
   * <ul>
   *   <li>When {@code Root}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#topoSort(String, Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootTargetTableReturnAll_whenRoot_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.topoSort("Root", new Hashtable<>(), true));
  }

  /**
   * Test {@link Project#topoSort(String, Hashtable)} with {@code root}, {@code targetTable}.
   * <ul>
   *   <li>When {@code Root}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#topoSort(String, Hashtable)}
   */
  @Test
  public void testTopoSortWithRootTargetTable_whenRoot_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.topoSort("Root", new Hashtable<>()));
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(project.topoSort(new String[]{}, new Hashtable<>(), true).isEmpty());
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    // Act and Assert
    assertTrue(project.topoSort(new String[]{}, new Hashtable<>(), true).isEmpty());
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll3() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new NoBannerLogger());

    // Act and Assert
    assertTrue(project.topoSort(new String[]{}, new Hashtable<>(), true).isEmpty());
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll4() throws BuildException {
    // Arrange
    Project project = new Project();

    Target target = new Target();
    target.addDependency("Build sequence for target(s)");

    Hashtable<String, Target> targetTable = new Hashtable<>();
    targetTable.put("Build sequence for target(s)", target);

    // Act and Assert
    assertThrows(BuildException.class, () -> project.topoSort(new String[]{}, targetTable, true));
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <ul>
   *   <li>Given {@code ,}.</li>
   *   <li>Then return size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll_givenComma_thenReturnSizeIsTwo() throws BuildException {
    // Arrange
    Project project = new Project();

    Hashtable<String, Target> targetTable = new Hashtable<>();
    Target target = new Target();
    targetTable.put(",", target);
    targetTable.put("Build sequence for target(s)", new Target());

    // Act
    Vector<Target> actualTopoSortResult = project.topoSort(new String[]{}, targetTable, true);

    // Assert
    assertEquals(2, actualTopoSortResult.size());
    assertSame(target, actualTopoSortResult.get(1));
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll_givenProject_thenReturnEmpty() throws BuildException {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertTrue(project.topoSort(new String[]{}, new Hashtable<>(), true).isEmpty());
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code false}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll_givenProject_whenFalse_thenReturnEmpty() throws BuildException {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertTrue(project.topoSort(new String[]{}, new Hashtable<>(), false).isEmpty());
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <ul>
   *   <li>Given {@link Target#Target()} addDependency {@code ,}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll_givenTargetAddDependencyComma() throws BuildException {
    // Arrange
    Project project = new Project();

    Target target = new Target();
    target.addDependency(",");
    target.addDependency("Build sequence for target(s)");

    Hashtable<String, Target> targetTable = new Hashtable<>();
    targetTable.put("Build sequence for target(s)", target);

    // Act and Assert
    assertThrows(BuildException.class, () -> project.topoSort(new String[]{}, targetTable, true));
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll_givenTarget_thenReturnSizeIsOne() throws BuildException {
    // Arrange
    Project project = new Project();

    Hashtable<String, Target> targetTable = new Hashtable<>();
    Target target = new Target();
    targetTable.put("Build sequence for target(s)", target);

    // Act
    Vector<Target> actualTopoSortResult = project.topoSort(new String[]{}, targetTable, true);

    // Assert
    assertEquals(1, actualTopoSortResult.size());
    assertSame(target, actualTopoSortResult.get(0));
  }

  /**
   * Test {@link Project#topoSort(String[], Hashtable, boolean)} with {@code roots}, {@code targetTable}, {@code returnAll}.
   * <ul>
   *   <li>When array of {@link String} with {@code Roots}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#topoSort(String[], Hashtable, boolean)}
   */
  @Test
  public void testTopoSortWithRootsTargetTableReturnAll_whenArrayOfStringWithRoots() throws BuildException {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> project.topoSort(new String[]{"Roots"}, new Hashtable<>(), true));
  }

  /**
   * Test {@link Project#hasReference(String)}.
   * <p>
   * Method under test: {@link Project#hasReference(String)}
   */
  @Test
  public void testHasReference() {
    // Arrange, Act and Assert
    assertFalse((new Project()).hasReference("Key"));
  }

  /**
   * Test {@link Project#getCopyOfReferences()}.
   * <p>
   * Method under test: {@link Project#getCopyOfReferences()}
   */
  @Test
  public void testGetCopyOfReferences() {
    // Arrange, Act and Assert
    assertTrue((new Project()).getCopyOfReferences().isEmpty());
  }

  /**
   * Test {@link Project#getReference(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code Key}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getReference(String)}
   */
  @Test
  public void testGetReference_givenJavaLangObject_whenKey_thenReturnNull() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Key", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getReference("Key"));
  }

  /**
   * Test {@link Project#getReference(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getReference(String)}
   */
  @Test
  public void testGetReference_givenProjectAddBuildListenerAntClassLoader_thenReturnNull() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getReference("Key"));
  }

  /**
   * Test {@link Project#getReference(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@link MagicNames#REFID_PROPERTY_HELPER} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getReference(String)}
   */
  @Test
  public void testGetReference_givenProjectAddReferenceRefid_property_helperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");

    // Act and Assert
    assertNull(project.getReference("Key"));
  }

  /**
   * Test {@link Project#getReference(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Key} and {@link Target#Target()}.</li>
   *   <li>When {@code Key}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getReference(String)}
   */
  @Test
  public void testGetReference_givenProjectAddTargetKeyAndTarget_whenKey_thenReturnNull() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Key", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(project.getReference("Key"));
  }

  /**
   * Test {@link Project#getReference(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@code Key}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getReference(String)}
   */
  @Test
  public void testGetReference_givenProject_whenKey_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Project()).getReference("Key"));
  }

  /**
   * Test {@link Project#getElementName(Object)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getElementName(Object)}
   */
  @Test
  public void testGetElementName_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("class java.lang.String", project.getElementName("Element"));
  }

  /**
   * Test {@link Project#getElementName(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getElementName(Object)}
   */
  @Test
  public void testGetElementName_givenProject() {
    // Arrange, Act and Assert
    assertEquals("class java.lang.String", (new Project()).getElementName("Element"));
  }

  /**
   * Test {@link Project#getElementName(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getElementName(Object)}
   */
  @Test
  public void testGetElementName_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("class java.lang.String", project.getElementName("Element"));
  }

  /**
   * Test {@link Project#getElementName(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@link MagicNames#REFID_PROPERTY_HELPER} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getElementName(Object)}
   */
  @Test
  public void testGetElementName_givenProjectAddReferenceRefid_property_helperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("class java.lang.String", project.getElementName("Element"));
  }

  /**
   * Test {@link Project#getElementName(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getElementName(Object)}
   */
  @Test
  public void testGetElementName_givenProjectDefaultIsComponent_helper_reference() {
    // Arrange
    Project project = new Project();
    project.setDefault(ComponentHelper.COMPONENT_HELPER_REFERENCE);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("class java.lang.String", project.getElementName("Element"));
  }

  /**
   * Test {@link Project#getThreadTask(Thread)}.
   * <ul>
   *   <li>When {@link Thread#Thread()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Project#getThreadTask(Thread)}
   */
  @Test
  public void testGetThreadTask_whenThread_thenReturnNull() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNull(project.getThreadTask(new Thread()));
  }
}
