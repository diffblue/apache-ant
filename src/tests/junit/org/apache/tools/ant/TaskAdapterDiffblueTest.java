package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class TaskAdapterDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TaskAdapter#TaskAdapter()}
   *   <li>{@link TaskAdapter#setProxy(Object)}
   *   <li>{@link TaskAdapter#getProxy()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    TaskAdapter actualTaskAdapter = new TaskAdapter();
    actualTaskAdapter.setProxy("42");

    // Assert
    assertEquals("42", actualTaskAdapter.getProxy());
    Location location = actualTaskAdapter.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTaskAdapter.getDescription());
    assertNull(actualTaskAdapter.getTaskName());
    assertNull(actualTaskAdapter.getTaskType());
    assertNull(actualTaskAdapter.getProject());
    assertNull(actualTaskAdapter.getOwningTarget());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualTaskAdapter.isInvalid());
    RuntimeConfigurable expectedWrapper = actualTaskAdapter.getRuntimeConfigurableWrapper();
    assertSame(expectedWrapper, actualTaskAdapter.getWrapper());
  }

  /**
   * Test {@link TaskAdapter#TaskAdapter(Object)}.
   * <p>
   * Method under test: {@link TaskAdapter#TaskAdapter(Object)}
   */
  @Test
  public void testNewTaskAdapter() {
    // Arrange and Act
    TaskAdapter actualTaskAdapter = new TaskAdapter("Proxy");

    // Assert
    assertEquals("Proxy", actualTaskAdapter.getProxy());
    assertNull(actualTaskAdapter.getDescription());
    assertNull(actualTaskAdapter.getTaskName());
    assertNull(actualTaskAdapter.getTaskType());
    assertNull(actualTaskAdapter.getProject());
    assertNull(actualTaskAdapter.getOwningTarget());
    assertFalse(actualTaskAdapter.isInvalid());
  }

  /**
   * Test {@link TaskAdapter#checkTaskClass(Class, Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TaskAdapter#checkTaskClass(Class, Project)}
   */
  @Test
  public void testCheckTaskClass_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Class<Object> taskClass = Object.class;

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> TaskAdapter.checkTaskClass(taskClass, project));
  }

  /**
   * Test {@link TaskAdapter#checkTaskClass(Class, Project)}.
   * <ul>
   *   <li>Given {@code No public execute() in}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TaskAdapter#checkTaskClass(Class, Project)}
   */
  @Test
  public void testCheckTaskClass_givenNoPublicExecuteIn() {
    // Arrange
    Class<Object> taskClass = Object.class;

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("No public execute() in ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> TaskAdapter.checkTaskClass(taskClass, project));
  }

  /**
   * Test {@link TaskAdapter#checkTaskClass(Class, Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TaskAdapter#checkTaskClass(Class, Project)}
   */
  @Test
  public void testCheckTaskClass_whenProject_thenThrowBuildException() {
    // Arrange
    Class<Object> taskClass = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> TaskAdapter.checkTaskClass(taskClass, new Project()));
  }

  /**
   * Test {@link TaskAdapter#checkProxyClass(Class)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TaskAdapter#checkProxyClass(Class)}
   */
  @Test
  public void testCheckProxyClass_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("No public execute() in ", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter taskAdapter = new TaskAdapter();
    taskAdapter.setProject(project);
    Class<Object> proxyClass = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> taskAdapter.checkProxyClass(proxyClass));
  }

  /**
   * Test {@link TaskAdapter#checkProxyClass(Class)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TaskAdapter#checkProxyClass(Class)}
   */
  @Test
  public void testCheckProxyClass_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter taskAdapter = new TaskAdapter();
    taskAdapter.setProject(project);
    Class<Object> proxyClass = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> taskAdapter.checkProxyClass(proxyClass));
  }

  /**
   * Test {@link TaskAdapter#checkProxyClass(Class)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TaskAdapter#checkProxyClass(Class)}
   */
  @Test
  public void testCheckProxyClass_givenProjectAddBuildListenerExecutorTest() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new ExecutorTest());

    TaskAdapter taskAdapter = new TaskAdapter();
    taskAdapter.setProject(project);
    Class<Object> proxyClass = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> taskAdapter.checkProxyClass(proxyClass));
  }

  /**
   * Test {@link TaskAdapter#checkProxyClass(Class)}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TaskAdapter#checkProxyClass(Class)}
   */
  @Test
  public void testCheckProxyClass_givenTaskAdapterProjectIsProject_thenThrowBuildException() {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();
    taskAdapter.setProject(new Project());
    Class<Object> proxyClass = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> taskAdapter.checkProxyClass(proxyClass));
  }

  /**
   * Test {@link TaskAdapter#execute()}.
   * <p>
   * Method under test: {@link TaskAdapter#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    TaskAdapter taskAdapter = new TaskAdapter();
    UnknownElement unknownElement = new UnknownElement("setLocation");
    taskAdapter.setProxy(unknownElement);

    // Act
    taskAdapter.execute();

    // Assert that nothing has changed
    Object proxy = taskAdapter.getProxy();
    assertTrue(proxy instanceof UnknownElement);
    assertSame(unknownElement, ((UnknownElement) proxy).getRuntimeConfigurableWrapper().getProxy());
  }

  /**
   * Test {@link TaskAdapter#execute()}.
   * <p>
   * Method under test: {@link TaskAdapter#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("setLocation");
    unknownElement.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "setLocation"));
    unknownElement.setRealThing(new UnknownElement("setLocation"));

    TaskAdapter taskAdapter = new TaskAdapter();
    taskAdapter.setProxy(unknownElement);

    // Act
    taskAdapter.execute();

    // Assert
    Object proxy = taskAdapter.getProxy();
    assertTrue(proxy instanceof UnknownElement);
    assertNull(((UnknownElement) proxy).getRuntimeConfigurableWrapper().getProxy());
    assertNull(((UnknownElement) proxy).getRealThing());
    assertNull(((UnknownElement) proxy).getTask());
  }

  /**
   * Test {@link TaskAdapter#execute()}.
   * <p>
   * Method under test: {@link TaskAdapter#execute()}
   */
  @Test
  public void testExecute3() throws BuildException {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("setLocation");
    unknownElement.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "setLocation"));
    unknownElement.setRealThing(new DummyTaskWithoutDefaultConstructor(1));

    TaskAdapter taskAdapter = new TaskAdapter();
    taskAdapter.setProxy(unknownElement);

    // Act
    taskAdapter.execute();

    // Assert
    Object proxy = taskAdapter.getProxy();
    assertTrue(proxy instanceof UnknownElement);
    assertNull(((UnknownElement) proxy).getRuntimeConfigurableWrapper().getProxy());
    assertNull(((UnknownElement) proxy).getRealThing());
    assertNull(((UnknownElement) proxy).getTask());
  }

  /**
   * Test {@link TaskAdapter#execute()}.
   * <ul>
   *   <li>Given {@link PickOneTask} (default constructor) Action is {@code execute}.</li>
   *   <li>Then {@link TaskAdapter#TaskAdapter()} Proxy {@link PickOneTask}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TaskAdapter#execute()}
   */
  @Test
  public void testExecute_givenPickOneTaskActionIsExecute_thenTaskAdapterProxyPickOneTask() throws BuildException {
    // Arrange
    PickOneTask pickOneTask = new PickOneTask();
    pickOneTask.setAction("execute");

    TaskAdapter taskAdapter = new TaskAdapter();
    taskAdapter.setProxy(pickOneTask);

    // Act
    taskAdapter.execute();

    // Assert that nothing has changed
    Object proxy = taskAdapter.getProxy();
    assertTrue(proxy instanceof PickOneTask);
    assertSame(pickOneTask, ((PickOneTask) proxy).getRuntimeConfigurableWrapper().getProxy());
  }

  /**
   * Test {@link TaskAdapter#execute()}.
   * <ul>
   *   <li>Given {@link UnknownElement#UnknownElement(String)} with elementName is {@code setLocation} RealThing is {@code Real Thing}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TaskAdapter#execute()}
   */
  @Test
  public void testExecute_givenUnknownElementWithElementNameIsSetLocationRealThingIsRealThing() throws BuildException {
    // Arrange
    UnknownElement unknownElement = new UnknownElement("setLocation");
    unknownElement.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "setLocation"));
    unknownElement.setRealThing("Real Thing");

    TaskAdapter taskAdapter = new TaskAdapter();
    taskAdapter.setProxy(unknownElement);

    // Act
    taskAdapter.execute();

    // Assert
    Object proxy = taskAdapter.getProxy();
    assertTrue(proxy instanceof UnknownElement);
    assertNull(((UnknownElement) proxy).getRuntimeConfigurableWrapper().getProxy());
    assertNull(((UnknownElement) proxy).getRealThing());
    assertNull(((UnknownElement) proxy).getTask());
  }
}
