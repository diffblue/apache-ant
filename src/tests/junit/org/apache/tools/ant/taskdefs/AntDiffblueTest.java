package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.Ant.Reference;
import org.apache.tools.ant.taskdefs.Ant.TargetElement;
import org.junit.Test;

public class AntDiffblueTest {
  /**
   * Test {@link Ant#Ant()}.
   * <p>
   * Method under test: {@link Ant#Ant()}
   */
  @Test
  public void testNewAnt() {
    // Arrange and Act
    Ant actualAnt = new Ant();

    // Assert
    assertEquals("build.xml", actualAnt.getDefaultBuildFile());
    Location location = actualAnt.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAnt.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualAnt.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualAnt.getTaskName());
    assertNull(actualAnt.getTaskType());
    assertNull(actualAnt.getProject());
    assertNull(actualAnt.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualAnt, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link Ant#Ant(Task)}.
   * <ul>
   *   <li>When {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return DefaultBuildFile is {@code build.xml}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#Ant(Task)}
   */
  @Test
  public void testNewAnt_whenTaskAdapter_thenReturnDefaultBuildFileIsBuildXml() {
    // Arrange and Act
    Ant actualAnt = new Ant(new TaskAdapter());

    // Assert
    assertEquals("build.xml", actualAnt.getDefaultBuildFile());
    Location location = actualAnt.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAnt.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualAnt.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualAnt.getTaskName());
    assertNull(actualAnt.getTaskType());
    assertNull(actualAnt.getProject());
    assertNull(actualAnt.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualAnt, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link Ant#handleInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#handleInput(byte[], int, int)}
   */
  @Test
  public void testHandleInput_thenReturnThree() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    Ant ant = new Ant();
    ant.setProject(project);

    // Act and Assert
    assertEquals(3, ant.handleInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, ant.getNewProject().getDefaultInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Ant#setAntfile(String)}
   *   <li>{@link Ant#setDir(File)}
   *   <li>{@link Ant#setInheritAll(boolean)}
   *   <li>{@link Ant#setInheritRefs(boolean)}
   *   <li>{@link Ant#setOutput(String)}
   *   <li>{@link Ant#setUseNativeBasedir(boolean)}
   *   <li>{@link Ant#getDefaultBuildFile()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Ant ant = new Ant();

    // Act
    ant.setAntfile("Ant File");
    ant.setDir(Copy.NULL_FILE_PLACEHOLDER);
    ant.setInheritAll(true);
    ant.setInheritRefs(true);
    ant.setOutput("Output File");
    ant.setUseNativeBasedir(true);

    // Assert
    assertEquals("build.xml", ant.getDefaultBuildFile());
  }

  /**
   * Test Reference getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Reference}
   *   <li>{@link Reference#setToRefid(String)}
   *   <li>{@link Reference#getToRefid()}
   * </ul>
   */
  @Test
  public void testReferenceGettersAndSetters() {
    // Arrange and Act
    Reference actualReference = new Reference();
    actualReference.setToRefid("Targetid");

    // Assert
    assertEquals("Targetid", actualReference.getToRefid());
    assertNull(actualReference.getRefId());
    assertNull(actualReference.getProject());
  }

  /**
   * Test {@link Ant#setTarget(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#setTarget(String)}
   */
  @Test
  public void testSetTarget_whenEmptyString_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Ant()).setTarget(""));
  }

  /**
   * Test {@link Ant#createProperty()}.
   * <ul>
   *   <li>Given {@link Ant#Ant()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return not Project KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#createProperty()}
   */
  @Test
  public void testCreateProperty_givenAntProjectIsProject_thenReturnNotProjectKeepGoingMode() {
    // Arrange
    Ant ant = new Ant();
    ant.setProject(new Project());

    // Act
    Property actualCreatePropertyResult = ant.createProperty();

    // Assert
    Project project = actualCreatePropertyResult.getProject();
    assertFalse(project.isKeepGoingMode());
    assertTrue(project.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions,
        actualCreatePropertyResult.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link Ant#createProperty()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#createProperty()}
   */
  @Test
  public void testCreateProperty_givenProjectCoreLoaderIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    project.addBuildListener(new AntClassLoader());

    Ant ant = new Ant();
    ant.setProject(project);

    // Act
    Property actualCreatePropertyResult = ant.createProperty();

    // Assert
    Project project2 = actualCreatePropertyResult.getProject();
    assertFalse(project2.isKeepGoingMode());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions,
        actualCreatePropertyResult.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link Ant#createProperty()}.
   * <ul>
   *   <li>Then return not Project KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#createProperty()}
   */
  @Test
  public void testCreateProperty_thenReturnNotProjectKeepGoingMode() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Ant ant = new Ant();
    ant.setProject(project);

    // Act
    Property actualCreatePropertyResult = ant.createProperty();

    // Assert
    Project project2 = actualCreatePropertyResult.getProject();
    assertFalse(project2.isKeepGoingMode());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions,
        actualCreatePropertyResult.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link Ant#createProperty()}.
   * <ul>
   *   <li>Then return Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#createProperty()}
   */
  @Test
  public void testCreateProperty_thenReturnProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.ComponentHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Ant ant = new Ant();
    ant.setProject(project);

    // Act and Assert
    Project project2 = ant.createProperty().getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get("ant.ComponentHelper"));
    assertSame(typeClass, dataTypeDefinitions.get("ant.ComponentHelper"));
  }

  /**
   * Test {@link Ant#createProperty()}.
   * <ul>
   *   <li>Then return Project KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#createProperty()}
   */
  @Test
  public void testCreateProperty_thenReturnProjectKeepGoingMode() {
    // Arrange
    Project project = new Project();
    project.setKeepGoingMode(true);
    project.addBuildListener(new AntClassLoader());

    Ant ant = new Ant();
    ant.setProject(project);

    // Act
    Property actualCreatePropertyResult = ant.createProperty();

    // Assert
    Project project2 = actualCreatePropertyResult.getProject();
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project2.isKeepGoingMode());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions,
        actualCreatePropertyResult.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link Ant#addConfiguredTarget(TargetElement)}.
   * <ul>
   *   <li>Given {@link Ant#Ant()} Target is {@code Target To Add}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#addConfiguredTarget(TargetElement)}
   */
  @Test
  public void testAddConfiguredTarget_givenAntTargetIsTargetToAdd_thenThrowBuildException() {
    // Arrange
    Ant ant = new Ant();
    ant.setTarget("Target To Add");

    TargetElement t = new TargetElement();
    t.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(BuildException.class, () -> ant.addConfiguredTarget(t));
  }

  /**
   * Test {@link Ant#addConfiguredTarget(TargetElement)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link TargetElement} (default constructor) Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#addConfiguredTarget(TargetElement)}
   */
  @Test
  public void testAddConfiguredTarget_givenEmptyString_whenTargetElementNameIsEmptyString() {
    // Arrange
    Ant ant = new Ant();

    TargetElement t = new TargetElement();
    t.setName("");

    // Act and Assert
    assertThrows(BuildException.class, () -> ant.addConfiguredTarget(t));
  }

  /**
   * Test {@link Ant#getNewProject()}.
   * <ul>
   *   <li>Given {@link Ant#Ant()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return not KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#getNewProject()}
   */
  @Test
  public void testGetNewProject_givenAntProjectIsProject_thenReturnNotKeepGoingMode() {
    // Arrange
    Ant ant = new Ant();
    ant.setProject(new Project());

    // Act
    Project actualNewProject = ant.getNewProject();

    // Assert
    assertFalse(actualNewProject.isKeepGoingMode());
    assertTrue(actualNewProject.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualNewProject.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getFilters());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getTargets());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getUserProperties());
  }

  /**
   * Test {@link Ant#getNewProject()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#getNewProject()}
   */
  @Test
  public void testGetNewProject_givenJavaLangObject_thenReturnDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.ComponentHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Ant ant = new Ant();
    ant.setProject(project);

    // Act
    Project actualNewProject = ant.getNewProject();

    // Assert
    Hashtable<String, Class<?>> dataTypeDefinitions = actualNewProject.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualNewProject.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get("ant.ComponentHelper"));
    assertSame(typeClass, dataTypeDefinitions.get("ant.ComponentHelper"));
  }

  /**
   * Test {@link Ant#getNewProject()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#getNewProject()}
   */
  @Test
  public void testGetNewProject_givenProjectCoreLoaderIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    project.addBuildListener(new AntClassLoader());

    Ant ant = new Ant();
    ant.setProject(project);

    // Act
    Project actualNewProject = ant.getNewProject();

    // Assert
    assertFalse(actualNewProject.isKeepGoingMode());
    assertTrue(actualNewProject.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualNewProject.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getFilters());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getTargets());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getUserProperties());
  }

  /**
   * Test {@link Ant#getNewProject()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) KeepGoingMode is {@code true}.</li>
   *   <li>Then return KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#getNewProject()}
   */
  @Test
  public void testGetNewProject_givenProjectKeepGoingModeIsTrue_thenReturnKeepGoingMode() {
    // Arrange
    Project project = new Project();
    project.setKeepGoingMode(true);
    project.addBuildListener(new AntClassLoader());

    Ant ant = new Ant();
    ant.setProject(project);

    // Act
    Project actualNewProject = ant.getNewProject();

    // Assert
    assertTrue(actualNewProject.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualNewProject.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(actualNewProject.isKeepGoingMode());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getFilters());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getTargets());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getUserProperties());
  }

  /**
   * Test {@link Ant#getNewProject()}.
   * <ul>
   *   <li>Then return not KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ant#getNewProject()}
   */
  @Test
  public void testGetNewProject_thenReturnNotKeepGoingMode() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Ant ant = new Ant();
    ant.setProject(project);

    // Act
    Project actualNewProject = ant.getNewProject();

    // Assert
    assertFalse(actualNewProject.isKeepGoingMode());
    assertTrue(actualNewProject.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = actualNewProject.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getFilters());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getTargets());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, actualNewProject.getUserProperties());
  }

  /**
   * Test TargetElement getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link TargetElement}
   *   <li>{@link TargetElement#setName(String)}
   *   <li>{@link TargetElement#getName()}
   * </ul>
   */
  @Test
  public void testTargetElementGettersAndSetters() {
    // Arrange and Act
    TargetElement actualTargetElement = new TargetElement();
    actualTargetElement.setName(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertEquals(Manifest.ATTRIBUTE_NAME, actualTargetElement.getName());
  }
}
