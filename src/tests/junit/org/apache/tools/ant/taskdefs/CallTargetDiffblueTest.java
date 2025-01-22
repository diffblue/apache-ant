package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class CallTargetDiffblueTest {
  /**
   * Test {@link CallTarget#execute()}.
   * <ul>
   *   <li>Given {@link CallTarget} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#execute()}
   */
  @Test
  public void testExecute_givenCallTargetProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    CallTarget callTarget = new CallTarget();
    callTarget.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> callTarget.execute());
  }

  /**
   * Test {@link CallTarget#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.ComponentHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    CallTarget callTarget = new CallTarget();
    callTarget.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> callTarget.execute());
  }

  /**
   * Test {@link CallTarget#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    CallTarget callTarget = new CallTarget();
    callTarget.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> callTarget.execute());
  }

  /**
   * Test {@link CallTarget#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#execute()}
   */
  @Test
  public void testExecute_givenProjectCoreLoaderIsAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    project.addBuildListener(new AntClassLoader());

    CallTarget callTarget = new CallTarget();
    callTarget.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> callTarget.execute());
  }

  /**
   * Test {@link CallTarget#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) KeepGoingMode is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#execute()}
   */
  @Test
  public void testExecute_givenProjectKeepGoingModeIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setKeepGoingMode(true);
    project.addBuildListener(new AntClassLoader());

    CallTarget callTarget = new CallTarget();
    callTarget.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> callTarget.execute());
  }

  /**
   * Test {@link CallTarget#createParam()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#createParam()}
   */
  @Test
  public void testCreateParam_givenProjectCoreLoaderIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    project.addBuildListener(new AntClassLoader());

    CallTarget callTarget = new CallTarget();
    callTarget.setProject(project);

    // Act
    Property actualCreateParamResult = callTarget.createParam();

    // Assert
    Project project2 = actualCreateParamResult.getProject();
    assertFalse(project2.isKeepGoingMode());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, actualCreateParamResult.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link CallTarget#createParam()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) KeepGoingMode is {@code true}.</li>
   *   <li>Then return Project KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#createParam()}
   */
  @Test
  public void testCreateParam_givenProjectKeepGoingModeIsTrue_thenReturnProjectKeepGoingMode() {
    // Arrange
    Project project = new Project();
    project.setKeepGoingMode(true);
    project.addBuildListener(new AntClassLoader());

    CallTarget callTarget = new CallTarget();
    callTarget.setProject(project);

    // Act
    Property actualCreateParamResult = callTarget.createParam();

    // Assert
    Project project2 = actualCreateParamResult.getProject();
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project2.isKeepGoingMode());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, actualCreateParamResult.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link CallTarget#createParam()}.
   * <ul>
   *   <li>Then return not Project KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#createParam()}
   */
  @Test
  public void testCreateParam_thenReturnNotProjectKeepGoingMode() {
    // Arrange
    CallTarget callTarget = new CallTarget();
    callTarget.setProject(new Project());

    // Act
    Property actualCreateParamResult = callTarget.createParam();

    // Assert
    Project project = actualCreateParamResult.getProject();
    assertFalse(project.isKeepGoingMode());
    assertTrue(project.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, actualCreateParamResult.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link CallTarget#createParam()}.
   * <ul>
   *   <li>Then return not Project KeepGoingMode.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#createParam()}
   */
  @Test
  public void testCreateParam_thenReturnNotProjectKeepGoingMode2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    CallTarget callTarget = new CallTarget();
    callTarget.setProject(project);

    // Act
    Property actualCreateParamResult = callTarget.createParam();

    // Assert
    Project project2 = actualCreateParamResult.getProject();
    assertFalse(project2.isKeepGoingMode());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTargets());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertEquals(copyOfDataTypeDefinitions, actualCreateParamResult.getRuntimeConfigurableWrapper().getAttributeMap());
  }

  /**
   * Test {@link CallTarget#createParam()}.
   * <ul>
   *   <li>Then return Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#createParam()}
   */
  @Test
  public void testCreateParam_thenReturnProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.ComponentHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    CallTarget callTarget = new CallTarget();
    callTarget.setProject(project);

    // Act and Assert
    Project project2 = callTarget.createParam().getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get("ant.ComponentHelper"));
    assertSame(typeClass, dataTypeDefinitions.get("ant.ComponentHelper"));
  }

  /**
   * Test {@link CallTarget#handleInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link CallTarget#handleInput(byte[], int, int)}
   */
  @Test
  public void testHandleInput_thenReturnThree() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    CallTarget callTarget = new CallTarget();
    callTarget.setProject(project);

    // Act and Assert
    assertEquals(3, callTarget.handleInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, callTarget.getProject().getDefaultInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test new {@link CallTarget} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CallTarget}
   */
  @Test
  public void testNewCallTarget() {
    // Arrange and Act
    CallTarget actualCallTarget = new CallTarget();

    // Assert
    Location location = actualCallTarget.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCallTarget.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualCallTarget.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualCallTarget.getTaskName());
    assertNull(actualCallTarget.getTaskType());
    assertNull(actualCallTarget.getProject());
    assertNull(actualCallTarget.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualCallTarget, runtimeConfigurableWrapper.getProxy());
  }
}
