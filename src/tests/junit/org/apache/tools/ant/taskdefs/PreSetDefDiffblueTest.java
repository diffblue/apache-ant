package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.AntTypeDefinition;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.UnknownElement;
import org.apache.tools.ant.taskdefs.PreSetDef.PreSetDefinition;
import org.junit.Test;

public class PreSetDefDiffblueTest {
  /**
   * Test {@link PreSetDef#addTask(Task)}.
   * <p>
   * Method under test: {@link PreSetDef#addTask(Task)}
   */
  @Test
  public void testAddTask() {
    // Arrange
    PreSetDef preSetDef = new PreSetDef();
    preSetDef.addTask(new UnknownElement("addTask called with a task that is not an unknown element"));

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDef.addTask(new TaskAdapter()));
  }

  /**
   * Test {@link PreSetDef#addTask(Task)}.
   * <ul>
   *   <li>Given {@link PreSetDef} (default constructor).</li>
   *   <li>When {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDef#addTask(Task)}
   */
  @Test
  public void testAddTask_givenPreSetDef_whenTaskAdapter_thenThrowBuildException() {
    // Arrange
    PreSetDef preSetDef = new PreSetDef();

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDef.addTask(new TaskAdapter()));
  }

  /**
   * Test {@link PreSetDef#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDef#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    UnknownElement nestedTask = new UnknownElement("Element Name");
    nestedTask.setNamespace("Namespace");

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    PreSetDef preSetDef = new PreSetDef();
    preSetDef.setProject(project);
    preSetDef.setName("Name not specified");
    preSetDef.addTask(nestedTask);

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDef.execute());
  }

  /**
   * Test {@link PreSetDef#execute()}.
   * <ul>
   *   <li>Given {@link PreSetDef} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDef#execute()}
   */
  @Test
  public void testExecute_givenPreSetDefProjectIsProject_thenThrowBuildException() {
    // Arrange
    UnknownElement nestedTask = new UnknownElement("Element Name");
    nestedTask.setNamespace("Namespace");

    PreSetDef preSetDef = new PreSetDef();
    preSetDef.setProject(new Project());
    preSetDef.setName("Name not specified");
    preSetDef.addTask(nestedTask);

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDef.execute());
  }

  /**
   * Test {@link PreSetDef#execute()}.
   * <ul>
   *   <li>Given {@link PreSetDef} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDef#execute()}
   */
  @Test
  public void testExecute_givenPreSetDef_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PreSetDef()).execute());
  }

  /**
   * Test {@link PreSetDef#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDef#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() {
    // Arrange
    UnknownElement nestedTask = new UnknownElement("Element Name");
    nestedTask.setNamespace("Namespace");

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PreSetDef preSetDef = new PreSetDef();
    preSetDef.setProject(project);
    preSetDef.setName("Name not specified");
    preSetDef.addTask(nestedTask);

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDef.execute());
  }

  /**
   * Test {@link PreSetDef#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDef#execute()}
   */
  @Test
  public void testExecute_givenProjectAddReferenceAntPropertyHelperAndValue() {
    // Arrange
    UnknownElement nestedTask = new UnknownElement("Element Name");
    nestedTask.setNamespace("Namespace");

    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    PreSetDef preSetDef = new PreSetDef();
    preSetDef.setProject(project);
    preSetDef.setName("Name not specified");
    preSetDef.addTask(nestedTask);

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDef.execute());
  }

  /**
   * Test {@link PreSetDef#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@code ant.ComponentHelper}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDef#execute()}
   */
  @Test
  public void testExecute_givenProjectDefaultIsAntComponentHelper_thenThrowBuildException() {
    // Arrange
    UnknownElement nestedTask = new UnknownElement("Element Name");
    nestedTask.setNamespace("Namespace");

    Project project = new Project();
    project.setDefault("ant.ComponentHelper");
    project.addBuildListener(new AntClassLoader());

    PreSetDef preSetDef = new PreSetDef();
    preSetDef.setProject(project);
    preSetDef.setName("Name not specified");
    preSetDef.addTask(nestedTask);

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDef.execute());
  }

  /**
   * Test {@link PreSetDef#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDef#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() {
    // Arrange
    PreSetDef preSetDef = new PreSetDef();
    preSetDef.addTask(new UnknownElement("Element Name"));

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDef.execute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link PreSetDef}
   *   <li>{@link PreSetDef#setName(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    PreSetDef actualPreSetDef = new PreSetDef();
    actualPreSetDef.setName(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertEquals("", actualPreSetDef.getURI());
    assertNull(actualPreSetDef.getAntlibClassLoader());
    Location location = actualPreSetDef.getLocation();
    assertNull(location.getFileName());
    assertNull(actualPreSetDef.getDescription());
    assertNull(actualPreSetDef.getTaskName());
    assertNull(actualPreSetDef.getTaskType());
    assertNull(actualPreSetDef.getProject());
    assertNull(actualPreSetDef.getOwningTarget());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#create(Project)}.
   * <p>
   * Method under test: {@link PreSetDefinition#create(Project)}
   */
  @Test
  public void testPreSetDefinitionCreate() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act and Assert
    assertSame(preSetDefinition, preSetDefinition.create(new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#createObject(Project)}.
   * <ul>
   *   <li>Given {@code ) for type}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#createObject(Project)}
   */
  @Test
  public void testPreSetDefinitionCreateObject_givenForType() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("Class Name");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(") for type ", typeClass);

    // Act and Assert
    assertNull(preSetDefinition.createObject(project));
    assertTrue(project.getBuildListeners().isEmpty());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#createObject(Project)}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#createObject(Project)}
   */
  @Test
  public void testPreSetDefinitionCreateObject_thenProjectBuildListenersSizeIsOne() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("org.apache.tools.ant.AntClassLoader");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    Project project = new Project();

    // Act
    preSetDefinition.createObject(project);

    // Assert
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#createObject(Project)}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#createObject(Project)}
   */
  @Test
  public void testPreSetDefinitionCreateObject_thenProjectBuildListenersSizeIsOne2() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("Class Name");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(preSetDefinition.createObject(project));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#createObject(Project)}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#createObject(Project)}
   */
  @Test
  public void testPreSetDefinitionCreateObject_thenProjectBuildListenersSizeIsTwo() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("org.apache.tools.ant.AntClassLoader");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    preSetDefinition.createObject(project);

    // Assert
    assertEquals(2, project.getBuildListeners().size());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#createObject(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#createObject(Project)}
   */
  @Test
  public void testPreSetDefinitionCreateObject_whenProject_thenProjectBuildListenersEmpty() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("Class Name");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    Project project = new Project();

    // Act and Assert
    assertNull(preSetDefinition.createObject(project));
    assertTrue(project.getBuildListeners().isEmpty());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getClassLoader()}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getClassLoader()}
   */
  @Test
  public void testPreSetDefinitionGetClassLoader_thenReturnNull() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();

    // Act and Assert
    assertNull((new PreSetDefinition(parent, new UnknownElement("Element Name"))).getClassLoader());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getClassName()}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getClassName()}
   */
  @Test
  public void testPreSetDefinitionGetClassName_thenReturnNull() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();

    // Act and Assert
    assertNull((new PreSetDefinition(parent, new UnknownElement("Element Name"))).getClassName());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getExposedClass(Project)}
   */
  @Test
  public void testPreSetDefinitionGetExposedClass_givenAntClassLoader() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("Class Name");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(preSetDefinition.getExposedClass(project));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Given {@code ) for type}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getExposedClass(Project)}
   */
  @Test
  public void testPreSetDefinitionGetExposedClass_givenForType() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("Class Name");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(") for type ", typeClass);

    // Act and Assert
    assertNull(preSetDefinition.getExposedClass(project));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Then return {@link AntClassLoader}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getExposedClass(Project)}
   */
  @Test
  public void testPreSetDefinitionGetExposedClass_thenReturnAntClassLoader() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("org.apache.tools.ant.AntClassLoader");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act
    Class<?> actualExposedClass = preSetDefinition.getExposedClass(new Project());

    // Assert
    Class<AntClassLoader> expectedExposedClass = AntClassLoader.class;
    assertEquals(expectedExposedClass, actualExposedClass);
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getExposedClass(Project)}
   */
  @Test
  public void testPreSetDefinitionGetExposedClass_thenReturnNull() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("Class Name");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act and Assert
    assertNull(preSetDefinition.getExposedClass(new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Then return {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getExposedClass(Project)}
   */
  @Test
  public void testPreSetDefinitionGetExposedClass_thenReturnObject() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    Class<Object> adapterClass = Object.class;
    parent.setAdapterClass(adapterClass);
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act
    Class<?> actualExposedClass = preSetDefinition.getExposedClass(new Project());

    // Assert
    Class<Object> expectedExposedClass = Object.class;
    assertEquals(expectedExposedClass, actualExposedClass);
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getPreSets()}.
   * <p>
   * Method under test: {@link PreSetDefinition#getPreSets()}
   */
  @Test
  public void testPreSetDefinitionGetPreSets() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    UnknownElement el = new UnknownElement("Element Name");

    // Act and Assert
    assertSame(el, (new PreSetDefinition(parent, el)).getPreSets());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getTypeClass(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getTypeClass(Project)}
   */
  @Test
  public void testPreSetDefinitionGetTypeClass_givenAntClassLoader() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("Class Name");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(preSetDefinition.getTypeClass(project));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getTypeClass(Project)}.
   * <ul>
   *   <li>Given {@code ) for type}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getTypeClass(Project)}
   */
  @Test
  public void testPreSetDefinitionGetTypeClass_givenForType() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("Class Name");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(") for type ", typeClass);

    // Act and Assert
    assertNull(preSetDefinition.getTypeClass(project));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getTypeClass(Project)}.
   * <ul>
   *   <li>Then return {@link AntClassLoader}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getTypeClass(Project)}
   */
  @Test
  public void testPreSetDefinitionGetTypeClass_thenReturnAntClassLoader() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("org.apache.tools.ant.AntClassLoader");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act
    Class<?> actualTypeClass = preSetDefinition.getTypeClass(new Project());

    // Assert
    Class<AntClassLoader> expectedTypeClass = AntClassLoader.class;
    assertEquals(expectedTypeClass, actualTypeClass);
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#getTypeClass(Project)}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#getTypeClass(Project)}
   */
  @Test
  public void testPreSetDefinitionGetTypeClass_thenReturnNull() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("Class Name");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act and Assert
    assertNull(preSetDefinition.getTypeClass(new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#PreSetDefinition(AntTypeDefinition, UnknownElement)}.
   * <ul>
   *   <li>Then return ClassLoader is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#PreSetDefinition(AntTypeDefinition, UnknownElement)}
   */
  @Test
  public void testPreSetDefinitionNewPreSetDefinition_thenReturnClassLoaderIsNull() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    UnknownElement el = new UnknownElement("Element Name");

    // Act
    PreSetDefinition actualPreSetDefinition = new PreSetDefinition(parent, el);

    // Assert
    assertNull(actualPreSetDefinition.getClassLoader());
    assertNull(actualPreSetDefinition.getName());
    assertNull(actualPreSetDefinition.getClassName());
    assertFalse(actualPreSetDefinition.isRestrict());
    assertSame(el, actualPreSetDefinition.getPreSets());
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#sameDefinition(AntTypeDefinition, Project)}.
   * <p>
   * Method under test: {@link PreSetDefinition#sameDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSameDefinition() {
    // Arrange
    PreSetDefinition preSetDefinition = new PreSetDefinition(null, new UnknownElement("Element Name"));
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition other = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act and Assert
    assertFalse(preSetDefinition.sameDefinition(other, new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#sameDefinition(AntTypeDefinition, Project)}.
   * <p>
   * Method under test: {@link PreSetDefinition#sameDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSameDefinition2() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    PreSetDefinition other = new PreSetDefinition(null, new UnknownElement("Element Name"));

    // Act and Assert
    assertFalse(preSetDefinition.sameDefinition(other, new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#sameDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>When {@link AntTypeDefinition} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#sameDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSameDefinition_whenAntTypeDefinition_thenReturnFalse() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    AntTypeDefinition other = new AntTypeDefinition();

    // Act and Assert
    assertFalse(preSetDefinition.sameDefinition(other, new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#sameDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#sameDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSameDefinition_whenNull_thenReturnFalse() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act and Assert
    assertFalse(preSetDefinition.sameDefinition(null, new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#setAdaptToClass(Class)}.
   * <p>
   * Method under test: {@link PreSetDefinition#setAdaptToClass(Class)}
   */
  @Test
  public void testPreSetDefinitionSetAdaptToClass() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    Class<Object> adaptToClass = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDefinition.setAdaptToClass(adaptToClass));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#setAdapterClass(Class)}.
   * <p>
   * Method under test: {@link PreSetDefinition#setAdapterClass(Class)}
   */
  @Test
  public void testPreSetDefinitionSetAdapterClass() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    Class<Object> adapterClass = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDefinition.setAdapterClass(adapterClass));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#setClass(Class)}.
   * <p>
   * Method under test: {@link PreSetDefinition#setClass(Class)}
   */
  @Test
  public void testPreSetDefinitionSetClass() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDefinition.setClass(clazz));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#setClassLoader(ClassLoader)}.
   * <p>
   * Method under test: {@link PreSetDefinition#setClassLoader(ClassLoader)}
   */
  @Test
  public void testPreSetDefinitionSetClassLoader() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act and Assert
    assertThrows(BuildException.class, () -> preSetDefinition.setClassLoader(new AntClassLoader()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#setClassName(String)}.
   * <p>
   * Method under test: {@link PreSetDefinition#setClassName(String)}
   */
  @Test
  public void testPreSetDefinitionSetClassName() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> (new PreSetDefinition(parent, new UnknownElement("Element Name"))).setClassName("Class Name"));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <p>
   * Method under test: {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSimilarDefinition() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("org.apache.tools.ant.taskdefs.PreSetDef$PreSetDefinition");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    AntTypeDefinition parent2 = new AntTypeDefinition();
    PreSetDefinition other = new PreSetDefinition(parent2,
        new UnknownElement("org.apache.tools.ant.AntTypeDefinition"));

    // Act and Assert
    assertFalse(preSetDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <p>
   * Method under test: {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSimilarDefinition2() {
    // Arrange
    PreSetDefinition preSetDefinition = new PreSetDefinition(null, new UnknownElement("Element Name"));
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition other = new PreSetDefinition(parent, new UnknownElement("org.apache.tools.ant.AntTypeDefinition"));

    // Act and Assert
    assertFalse(preSetDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <p>
   * Method under test: {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSimilarDefinition3() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    PreSetDefinition other = new PreSetDefinition(null, new UnknownElement("org.apache.tools.ant.AntTypeDefinition"));

    // Act and Assert
    assertFalse(preSetDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <p>
   * Method under test: {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSimilarDefinition4() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    parent.setClassName("org.apache.tools.ant.taskdefs.PreSetDef$PreSetDefinition");
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    AntTypeDefinition parent2 = new AntTypeDefinition();
    parent2.setClassName("org.apache.tools.ant.taskdefs.PreSetDef$PreSetDefinition");
    PreSetDefinition other = new PreSetDefinition(parent2,
        new UnknownElement("org.apache.tools.ant.AntTypeDefinition"));

    // Act and Assert
    assertFalse(preSetDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>When {@link AntTypeDefinition} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSimilarDefinition_whenAntTypeDefinition_thenReturnFalse() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    AntTypeDefinition other = new AntTypeDefinition();

    // Act and Assert
    assertFalse(preSetDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test PreSetDefinition {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PreSetDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testPreSetDefinitionSimilarDefinition_whenNull_thenReturnFalse() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act and Assert
    assertFalse(preSetDefinition.similarDefinition(null, new Project()));
  }
}
