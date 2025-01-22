package org.apache.tools.ant;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;
import org.apache.tools.ant.LocationTest.EchoLocation;
import org.apache.tools.ant.taskdefs.DefaultExcludes;
import org.junit.Test;

public class UnknownElementDiffblueTest {
  /**
   * Test {@link UnknownElement#UnknownElement(String)}.
   * <p>
   * Method under test: {@link UnknownElement#UnknownElement(String)}
   */
  @Test
  public void testNewUnknownElement() {
    // Arrange and Act
    UnknownElement actualUnknownElement = new UnknownElement("Element Name");

    // Assert
    assertEquals("", actualUnknownElement.getNamespace());
    assertEquals("Element Name", actualUnknownElement.getComponentName());
    assertEquals("Element Name", actualUnknownElement.getTag());
    assertNull(actualUnknownElement.getRealThing());
    assertNull(actualUnknownElement.getDescription());
    assertNull(actualUnknownElement.getTaskType());
    assertNull(actualUnknownElement.getQName());
    assertNull(actualUnknownElement.getTaskName());
    assertNull(actualUnknownElement.getChildren());
    assertNull(actualUnknownElement.getProject());
    assertNull(actualUnknownElement.getOwningTarget());
    assertNull(actualUnknownElement.getTask());
    assertFalse(actualUnknownElement.isInvalid());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link UnknownElement#setQName(String)}
   *   <li>{@link UnknownElement#setRealThing(Object)}
   *   <li>{@link UnknownElement#getChildren()}
   *   <li>{@link UnknownElement#getNamespace()}
   *   <li>{@link UnknownElement#getQName()}
   *   <li>{@link UnknownElement#getRealThing()}
   *   <li>{@link UnknownElement#getTag()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");

    // Act
    unknownElement.setQName("Qname");
    unknownElement.setRealThing("Real Thing");
    List<UnknownElement> actualChildren = unknownElement.getChildren();
    String actualNamespace = unknownElement.getNamespace();
    String actualQName = unknownElement.getQName();
    Object actualRealThing = unknownElement.getRealThing();

    // Assert
    assertEquals("", actualNamespace);
    assertEquals("Element Name", unknownElement.getTag());
    assertEquals("Qname", actualQName);
    assertEquals("Real Thing", actualRealThing);
    assertNull(actualChildren);
  }

  /**
   * Test {@link UnknownElement#setNamespace(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#setNamespace(String)}
   */
  @Test
  public void testSetNamespace_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(ComponentHelper.COMPONENT_HELPER_REFERENCE, typeClass);
    project.addBuildListener(new AntClassLoader());

    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setProject(project);

    // Act
    unknownElement.setNamespace(ProjectHelper.ANT_CURRENT_URI);

    // Assert that nothing has changed
    assertEquals("", unknownElement.getNamespace());
    assertEquals("Element Name", unknownElement.getComponentName());
  }

  /**
   * Test {@link UnknownElement#setNamespace(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#setNamespace(String)}
   */
  @Test
  public void testSetNamespace_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setProject(project);

    // Act
    unknownElement.setNamespace(ProjectHelper.ANT_CURRENT_URI);

    // Assert that nothing has changed
    assertEquals("", unknownElement.getNamespace());
    assertEquals("Element Name", unknownElement.getComponentName());
  }

  /**
   * Test {@link UnknownElement#setNamespace(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@link ProjectHelper#ANT_CURRENT_URI}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#setNamespace(String)}
   */
  @Test
  public void testSetNamespace_givenProjectDefaultIsAnt_current_uri() {
    // Arrange
    Project project = new Project();
    project.setDefault(ProjectHelper.ANT_CURRENT_URI);
    project.addBuildListener(new AntClassLoader());

    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setProject(project);

    // Act
    unknownElement.setNamespace(ProjectHelper.ANT_CURRENT_URI);

    // Assert that nothing has changed
    assertEquals("", unknownElement.getNamespace());
    assertEquals("Element Name", unknownElement.getComponentName());
  }

  /**
   * Test {@link UnknownElement#setNamespace(String)}.
   * <ul>
   *   <li>Then {@link UnknownElement#UnknownElement(String)} with {@code Element Name} Namespace is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#setNamespace(String)}
   */
  @Test
  public void testSetNamespace_thenUnknownElementWithElementNameNamespaceIsEmptyString() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setProject(new Project());

    // Act
    unknownElement.setNamespace(ProjectHelper.ANT_CURRENT_URI);

    // Assert that nothing has changed
    assertEquals("", unknownElement.getNamespace());
    assertEquals("Element Name", unknownElement.getComponentName());
  }

  /**
   * Test {@link UnknownElement#setNamespace(String)}.
   * <ul>
   *   <li>Then {@link UnknownElement#UnknownElement(String)} with {@code Element Name} Namespace is {@code Namespace}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#setNamespace(String)}
   */
  @Test
  public void testSetNamespace_thenUnknownElementWithElementNameNamespaceIsNamespace() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");

    // Act
    unknownElement.setNamespace("Namespace");

    // Assert
    assertEquals("Namespace", unknownElement.getNamespace());
    assertEquals("Namespace:Element Name", unknownElement.getComponentName());
  }

  /**
   * Test {@link UnknownElement#getWrapper()}.
   * <p>
   * Method under test: {@link UnknownElement#getWrapper()}
   */
  @Test
  public void testGetWrapper() {
    // Arrange, Act and Assert
    assertNull((new UnknownElement("Element Name")).getWrapper());
  }

  /**
   * Test {@link UnknownElement#configure(Object)}.
   * <p>
   * Method under test: {@link UnknownElement#configure(Object)}
   */
  @Test
  public void testConfigure() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "Element Tag"));

    // Act
    unknownElement.configure("Real Object");

    // Assert
    assertEquals("Real Object", unknownElement.getRuntimeConfigurableWrapper().getProxy());
    assertEquals("Real Object", unknownElement.getRealThing());
  }

  /**
   * Test {@link UnknownElement#configure(Object)}.
   * <p>
   * Method under test: {@link UnknownElement#configure(Object)}
   */
  @Test
  public void testConfigure2() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");

    unknownElement.setRuntimeConfigurableWrapper(wrapper);

    // Act
    unknownElement.configure(null);

    // Assert that nothing has changed
    RuntimeConfigurable runtimeConfigurableWrapper = unknownElement.getRuntimeConfigurableWrapper();
    assertEquals("Proxy", runtimeConfigurableWrapper.getProxy());
    assertSame(wrapper, runtimeConfigurableWrapper);
    assertSame(wrapper, unknownElement.getWrapper());
  }

  /**
   * Test {@link UnknownElement#configure(Object)}.
   * <p>
   * Method under test: {@link UnknownElement#configure(Object)}
   */
  @Test
  public void testConfigure3() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    RuntimeConfigurable wrapper = new RuntimeConfigurable("Proxy", "Element Tag");

    unknownElement.setRuntimeConfigurableWrapper(wrapper);
    DefaultExcludes defaultExcludes = new DefaultExcludes();

    // Act
    unknownElement.configure(defaultExcludes);

    // Assert
    RuntimeConfigurable runtimeConfigurableWrapper = defaultExcludes.getRuntimeConfigurableWrapper();
    assertEquals("Element Tag", runtimeConfigurableWrapper.getElementTag());
    assertSame(wrapper, unknownElement.getRuntimeConfigurableWrapper());
    assertSame(wrapper, runtimeConfigurableWrapper);
    assertSame(wrapper, defaultExcludes.getWrapper());
    assertSame(wrapper, unknownElement.getWrapper());
    assertSame(defaultExcludes, unknownElement.getRealThing());
    assertSame(defaultExcludes, unknownElement.getTask());
  }

  /**
   * Test {@link UnknownElement#handleInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#handleInput(byte[], int, int)}
   */
  @Test
  public void testHandleInput_thenReturnThree() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setProject(project);

    // Act and Assert
    assertEquals(3, unknownElement.handleInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, unknownElement.getProject().getDefaultInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link UnknownElement#execute()}.
   * <p>
   * Method under test: {@link UnknownElement#execute()}
   */
  @Test
  public void testExecute() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");

    // Act
    unknownElement.execute();

    // Assert that nothing has changed
    assertSame(unknownElement, unknownElement.getRuntimeConfigurableWrapper().getProxy());
  }

  /**
   * Test {@link UnknownElement#execute()}.
   * <p>
   * Method under test: {@link UnknownElement#execute()}
   */
  @Test
  public void testExecute2() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "Element Tag"));
    unknownElement.setRealThing(new DummyTaskWithoutDefaultConstructor(1));

    // Act
    unknownElement.execute();

    // Assert
    assertNull(unknownElement.getRuntimeConfigurableWrapper().getProxy());
    assertNull(unknownElement.getRealThing());
    assertNull(unknownElement.getTask());
  }

  /**
   * Test {@link UnknownElement#execute()}.
   * <ul>
   *   <li>Given {@link UnknownElement#UnknownElement(String)} with {@code Element Name} RealThing is {@link EchoLocation} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#execute()}
   */
  @Test
  public void testExecute_givenUnknownElementWithElementNameRealThingIsEchoLocation() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "Element Tag"));
    unknownElement.setRealThing(new EchoLocation());

    // Act
    unknownElement.execute();

    // Assert
    assertNull(unknownElement.getRuntimeConfigurableWrapper().getProxy());
    assertNull(unknownElement.getRealThing());
    assertNull(unknownElement.getTask());
  }

  /**
   * Test {@link UnknownElement#execute()}.
   * <ul>
   *   <li>Given {@link UnknownElement#UnknownElement(String)} with {@code Element Name} RealThing is {@link PickOneTask} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#execute()}
   */
  @Test
  public void testExecute_givenUnknownElementWithElementNameRealThingIsPickOneTask() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "Element Tag"));
    unknownElement.setRealThing(new PickOneTask());

    // Act
    unknownElement.execute();

    // Assert
    assertNull(unknownElement.getRuntimeConfigurableWrapper().getProxy());
    assertNull(unknownElement.getRealThing());
    assertNull(unknownElement.getTask());
  }

  /**
   * Test {@link UnknownElement#execute()}.
   * <ul>
   *   <li>Given {@link UnknownElement#UnknownElement(String)} with {@code Element Name} RealThing is {@code Real Thing}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#execute()}
   */
  @Test
  public void testExecute_givenUnknownElementWithElementNameRealThingIsRealThing() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "Element Tag"));
    unknownElement.setRealThing("Real Thing");

    // Act
    unknownElement.execute();

    // Assert
    assertNull(unknownElement.getRuntimeConfigurableWrapper().getProxy());
    assertNull(unknownElement.getRealThing());
    assertNull(unknownElement.getTask());
  }

  /**
   * Test {@link UnknownElement#getComponentName()}.
   * <ul>
   *   <li>Given {@link UnknownElement#UnknownElement(String)} with {@code Element Name} Namespace is {@link ProjectHelper#ANT_CORE_URI}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#getComponentName()}
   */
  @Test
  public void testGetComponentName_givenUnknownElementWithElementNameNamespaceIsAnt_core_uri() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setNamespace(ProjectHelper.ANT_CORE_URI);

    // Act and Assert
    assertEquals("Element Name", unknownElement.getComponentName());
  }

  /**
   * Test {@link UnknownElement#getComponentName()}.
   * <ul>
   *   <li>Given {@link UnknownElement#UnknownElement(String)} with {@code Element Name}.</li>
   *   <li>Then return {@code Element Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#getComponentName()}
   */
  @Test
  public void testGetComponentName_givenUnknownElementWithElementName_thenReturnElementName() {
    // Arrange, Act and Assert
    assertEquals("Element Name", (new UnknownElement("Element Name")).getComponentName());
  }

  /**
   * Test {@link UnknownElement#getComponentName()}.
   * <ul>
   *   <li>Then return {@code foo:Element Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#getComponentName()}
   */
  @Test
  public void testGetComponentName_thenReturnFooElementName() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setNamespace("foo");

    // Act and Assert
    assertEquals("foo:Element Name", unknownElement.getComponentName());
  }

  /**
   * Test {@link UnknownElement#getTaskName()}.
   * <p>
   * Method under test: {@link UnknownElement#getTaskName()}
   */
  @Test
  public void testGetTaskName() {
    // Arrange, Act and Assert
    assertNull((new UnknownElement("Element Name")).getTaskName());
  }

  /**
   * Test {@link UnknownElement#getTask()}.
   * <p>
   * Method under test: {@link UnknownElement#getTask()}
   */
  @Test
  public void testGetTask() {
    // Arrange, Act and Assert
    assertNull((new UnknownElement("Element Name")).getTask());
  }

  /**
   * Test {@link UnknownElement#similar(Object)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#similar(Object)}
   */
  @Test
  public void testSimilar_whenNull() {
    // Arrange, Act and Assert
    assertFalse((new UnknownElement("Element Name")).similar(null));
  }

  /**
   * Test {@link UnknownElement#similar(Object)}.
   * <ul>
   *   <li>When {@code Obj}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#similar(Object)}
   */
  @Test
  public void testSimilar_whenObj() {
    // Arrange, Act and Assert
    assertFalse((new UnknownElement("Element Name")).similar("Obj"));
  }

  /**
   * Test {@link UnknownElement#copy(Project)}.
   * <ul>
   *   <li>Then return Namespace is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnknownElement#copy(Project)}
   */
  @Test
  public void testCopy_thenReturnNamespaceIsEmptyString() {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("Element Name");
    unknownElement.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", ProjectHelper.ANT_CURRENT_URI));
    Project newProject = new Project();

    // Act
    UnknownElement actualCopyResult = unknownElement.copy(newProject);

    // Assert
    assertEquals("", actualCopyResult.getNamespace());
    assertEquals("Element Name", actualCopyResult.getComponentName());
    assertEquals("Element Name", actualCopyResult.getTag());
    assertNull(actualCopyResult.getRealThing());
    assertNull(actualCopyResult.getDescription());
    assertNull(actualCopyResult.getTaskType());
    assertNull(actualCopyResult.getQName());
    assertNull(actualCopyResult.getTaskName());
    assertNull(actualCopyResult.getChildren());
    assertNull(actualCopyResult.getTask());
    assertFalse(actualCopyResult.isInvalid());
    assertSame(newProject, actualCopyResult.getProject());
  }
}
