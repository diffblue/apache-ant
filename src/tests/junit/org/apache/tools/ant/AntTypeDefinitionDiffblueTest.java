package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.lang.reflect.InvocationTargetException;
import org.apache.tools.ant.taskdefs.PreSetDef;
import org.apache.tools.ant.taskdefs.PreSetDef.PreSetDefinition;
import org.junit.Test;

public class AntTypeDefinitionDiffblueTest {
  /**
   * Test {@link AntTypeDefinition#setClass(Class)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#setClass(Class)}
   */
  @Test
  public void testSetClass_givenAntTypeDefinition() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    Class<Object> clazz = Object.class;

    // Act
    antTypeDefinition.setClass(clazz);

    // Assert
    assertEquals("java.lang.Object", antTypeDefinition.getClassName());
  }

  /**
   * Test {@link AntTypeDefinition#setClass(Class)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) ClassLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#setClass(Class)}
   */
  @Test
  public void testSetClass_givenAntTypeDefinitionClassLoaderIsAntClassLoader() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassLoader(new AntClassLoader());
    antTypeDefinition.setClassName(null);
    Class<Object> clazz = Object.class;

    // Act
    antTypeDefinition.setClass(clazz);

    // Assert
    assertEquals("java.lang.Object", antTypeDefinition.getClassName());
  }

  /**
   * Test {@link AntTypeDefinition#setClass(Class)}.
   * <ul>
   *   <li>Then {@link AntTypeDefinition} (default constructor) ClassName is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#setClass(Class)}
   */
  @Test
  public void testSetClass_thenAntTypeDefinitionClassNameIsFoo() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassLoader(null);
    antTypeDefinition.setClassName("foo");
    Class<Object> clazz = Object.class;

    // Act
    antTypeDefinition.setClass(clazz);

    // Assert that nothing has changed
    assertEquals("foo", antTypeDefinition.getClassName());
  }

  /**
   * Test {@link AntTypeDefinition#setClass(Class)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link AntTypeDefinition} (default constructor) ClassName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#setClass(Class)}
   */
  @Test
  public void testSetClass_whenNull_thenAntTypeDefinitionClassNameIsNull() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassLoader(null);
    antTypeDefinition.setClassName(null);

    // Act
    antTypeDefinition.setClass(null);

    // Assert that nothing has changed
    assertNull(antTypeDefinition.getClassName());
  }

  /**
   * Test {@link AntTypeDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#getExposedClass(Project)}
   */
  @Test
  public void testGetExposedClass_givenAntClassLoader() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(antTypeDefinition.getExposedClass(project));
  }

  /**
   * Test {@link AntTypeDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) AdapterClass is {@link Object}.</li>
   *   <li>Then return {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#getExposedClass(Project)}
   */
  @Test
  public void testGetExposedClass_givenAntTypeDefinitionAdapterClassIsObject_thenReturnObject() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    Class<Object> adapterClass = Object.class;
    antTypeDefinition.setAdapterClass(adapterClass);

    // Act
    Class<?> actualExposedClass = antTypeDefinition.getExposedClass(new Project());

    // Assert
    Class<Object> expectedExposedClass = Object.class;
    assertEquals(expectedExposedClass, actualExposedClass);
  }

  /**
   * Test {@link AntTypeDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) ClassName is {@code Class Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#getExposedClass(Project)}
   */
  @Test
  public void testGetExposedClass_givenAntTypeDefinitionClassNameIsClassName_thenReturnNull() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    // Act and Assert
    assertNull(antTypeDefinition.getExposedClass(new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Given {@code ) for type}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#getExposedClass(Project)}
   */
  @Test
  public void testGetExposedClass_givenForType() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(") for type ", typeClass);

    // Act and Assert
    assertNull(antTypeDefinition.getExposedClass(project));
  }

  /**
   * Test {@link AntTypeDefinition#getExposedClass(Project)}.
   * <ul>
   *   <li>Then return {@link AntClassLoader}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#getExposedClass(Project)}
   */
  @Test
  public void testGetExposedClass_thenReturnAntClassLoader() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("org.apache.tools.ant.AntClassLoader");

    // Act
    Class<?> actualExposedClass = antTypeDefinition.getExposedClass(new Project());

    // Assert
    Class<AntClassLoader> expectedExposedClass = AntClassLoader.class;
    assertEquals(expectedExposedClass, actualExposedClass);
  }

  /**
   * Test {@link AntTypeDefinition#getTypeClass(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#getTypeClass(Project)}
   */
  @Test
  public void testGetTypeClass_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(antTypeDefinition.getTypeClass(project));
  }

  /**
   * Test {@link AntTypeDefinition#getTypeClass(Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) ClassName is {@code Class Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#getTypeClass(Project)}
   */
  @Test
  public void testGetTypeClass_givenAntTypeDefinitionClassNameIsClassName_thenReturnNull() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    // Act and Assert
    assertNull(antTypeDefinition.getTypeClass(new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#getTypeClass(Project)}.
   * <ul>
   *   <li>Given {@code ) for type}.</li>
   *   <li>When {@link Project} (default constructor) addDataTypeDefinition {@code ) for type} and {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#getTypeClass(Project)}
   */
  @Test
  public void testGetTypeClass_givenForType_whenProjectAddDataTypeDefinitionForTypeAndObject() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(") for type ", typeClass);

    // Act and Assert
    assertNull(antTypeDefinition.getTypeClass(project));
  }

  /**
   * Test {@link AntTypeDefinition#getTypeClass(Project)}.
   * <ul>
   *   <li>Then return {@link AntClassLoader}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#getTypeClass(Project)}
   */
  @Test
  public void testGetTypeClass_thenReturnAntClassLoader() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("org.apache.tools.ant.AntClassLoader");

    // Act
    Class<?> actualTypeClass = antTypeDefinition.getTypeClass(new Project());

    // Assert
    Class<AntClassLoader> expectedTypeClass = AntClassLoader.class;
    assertEquals(expectedTypeClass, actualTypeClass);
  }

  /**
   * Test {@link AntTypeDefinition#innerGetTypeClass()}.
   * <p>
   * Method under test: {@link AntTypeDefinition#innerGetTypeClass()}
   */
  @Test
  public void testInnerGetTypeClass() throws ClassNotFoundException {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("org.apache.tools.ant.AntClassLoader");
    antTypeDefinition.setClassLoader(new AntClassLoader(new AntClassLoader(), true));

    // Act
    Class<?> actualInnerGetTypeClassResult = antTypeDefinition.innerGetTypeClass();

    // Assert
    Class<AntClassLoader> expectedInnerGetTypeClassResult = AntClassLoader.class;
    assertEquals(expectedInnerGetTypeClassResult, actualInnerGetTypeClassResult);
  }

  /**
   * Test {@link AntTypeDefinition#innerGetTypeClass()}.
   * <p>
   * Method under test: {@link AntTypeDefinition#innerGetTypeClass()}
   */
  @Test
  public void testInnerGetTypeClass2() throws ClassNotFoundException {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("org.apache.tools.ant.AntClassLoader");
    antTypeDefinition.setClassLoader(new AntClassLoader(new AntClassLoader(), false));

    // Act
    Class<?> actualInnerGetTypeClassResult = antTypeDefinition.innerGetTypeClass();

    // Assert
    Class<AntClassLoader> expectedInnerGetTypeClassResult = AntClassLoader.class;
    assertEquals(expectedInnerGetTypeClassResult, actualInnerGetTypeClassResult);
  }

  /**
   * Test {@link AntTypeDefinition#innerGetTypeClass()}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) ClassLoader is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#innerGetTypeClass()}
   */
  @Test
  public void testInnerGetTypeClass_givenAntTypeDefinitionClassLoaderIsNull() throws ClassNotFoundException {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("org.apache.tools.ant.AntClassLoader");
    antTypeDefinition.setClassLoader(null);

    // Act
    Class<?> actualInnerGetTypeClassResult = antTypeDefinition.innerGetTypeClass();

    // Assert
    Class<AntClassLoader> expectedInnerGetTypeClassResult = AntClassLoader.class;
    assertEquals(expectedInnerGetTypeClassResult, actualInnerGetTypeClassResult);
  }

  /**
   * Test {@link AntTypeDefinition#innerGetTypeClass()}.
   * <ul>
   *   <li>Then return {@link AntClassLoader}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#innerGetTypeClass()}
   */
  @Test
  public void testInnerGetTypeClass_thenReturnAntClassLoader() throws ClassNotFoundException {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("org.apache.tools.ant.AntClassLoader");
    antTypeDefinition.setClassLoader(new AntClassLoader());

    // Act
    Class<?> actualInnerGetTypeClassResult = antTypeDefinition.innerGetTypeClass();

    // Assert
    Class<AntClassLoader> expectedInnerGetTypeClassResult = AntClassLoader.class;
    assertEquals(expectedInnerGetTypeClassResult, actualInnerGetTypeClassResult);
  }

  /**
   * Test {@link AntTypeDefinition#create(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#create(Project)}
   */
  @Test
  public void testCreate_givenAntClassLoader_thenProjectBuildListenersSizeIsTwo() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("org.apache.tools.ant.AntClassLoader");

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    antTypeDefinition.create(project);

    // Assert
    assertEquals(2, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntTypeDefinition#create(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#create(Project)}
   */
  @Test
  public void testCreate_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(antTypeDefinition.create(project));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntTypeDefinition#create(Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) ClassName is {@code AntClassLoader}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#create(Project)}
   */
  @Test
  public void testCreate_givenAntTypeDefinitionClassNameIsOrgApacheToolsAntAntClassLoader() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("org.apache.tools.ant.AntClassLoader");
    Project project = new Project();

    // Act
    antTypeDefinition.create(project);

    // Assert
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntTypeDefinition#create(Project)}.
   * <ul>
   *   <li>Given {@link DefaultLogger} (default constructor).</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#create(Project)}
   */
  @Test
  public void testCreate_givenDefaultLogger_whenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    // Act and Assert
    assertNull(antTypeDefinition.create(project));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntTypeDefinition#create(Project)}.
   * <ul>
   *   <li>Given {@link ExecutorTest} (default constructor).</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#create(Project)}
   */
  @Test
  public void testCreate_givenExecutorTest_whenProjectAddBuildListenerExecutorTest() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    project.addBuildListener(new ExecutorTest());

    // Act and Assert
    assertNull(antTypeDefinition.create(project));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntTypeDefinition#create(Project)}.
   * <ul>
   *   <li>Given {@code ) for type}.</li>
   *   <li>When {@link Project} (default constructor) addDataTypeDefinition {@code ) for type} and {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#create(Project)}
   */
  @Test
  public void testCreate_givenForType_whenProjectAddDataTypeDefinitionForTypeAndObject() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(") for type ", typeClass);

    // Act and Assert
    assertNull(antTypeDefinition.create(project));
    assertTrue(project.getBuildListeners().isEmpty());
  }

  /**
   * Test {@link AntTypeDefinition#create(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#create(Project)}
   */
  @Test
  public void testCreate_whenProject_thenProjectBuildListenersEmpty() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");
    Project project = new Project();

    // Act and Assert
    assertNull(antTypeDefinition.create(project));
    assertTrue(project.getBuildListeners().isEmpty());
  }

  /**
   * Test {@link AntTypeDefinition#checkClass(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#checkClass(Project)}
   */
  @Test
  public void testCheckClass_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> antTypeDefinition.checkClass(project));
  }

  /**
   * Test {@link AntTypeDefinition#checkClass(Project)}.
   * <ul>
   *   <li>Given {@code ) for type}.</li>
   *   <li>When {@link Project} (default constructor) addDataTypeDefinition {@code ) for type} and {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#checkClass(Project)}
   */
  @Test
  public void testCheckClass_givenForType_whenProjectAddDataTypeDefinitionForTypeAndObject() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(") for type ", typeClass);

    // Act and Assert
    assertThrows(BuildException.class, () -> antTypeDefinition.checkClass(project));
  }

  /**
   * Test {@link AntTypeDefinition#checkClass(Project)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#checkClass(Project)}
   */
  @Test
  public void testCheckClass_thenThrowBuildException() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    // Act and Assert
    assertThrows(BuildException.class, () -> antTypeDefinition.checkClass(new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#innerCreateAndSet(Class, Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#innerCreateAndSet(Class, Project)}
   */
  @Test
  public void testInnerCreateAndSet_givenAntClassLoader_thenProjectBuildListenersSizeIsTwo()
      throws IllegalAccessException, InstantiationException, NoSuchMethodException, InvocationTargetException {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    Class<AntClassLoader> newclass = AntClassLoader.class;

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    antTypeDefinition.innerCreateAndSet(newclass, project);

    // Assert
    assertEquals(2, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntTypeDefinition#innerCreateAndSet(Class, Project)}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#innerCreateAndSet(Class, Project)}
   */
  @Test
  public void testInnerCreateAndSet_thenProjectBuildListenersSizeIsOne()
      throws IllegalAccessException, InstantiationException, NoSuchMethodException, InvocationTargetException {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    Class<AntClassLoader> newclass = AntClassLoader.class;
    Project project = new Project();

    // Act
    antTypeDefinition.innerCreateAndSet(newclass, project);

    // Assert
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntTypeDefinition#innerCreateAndSet(Class, Project)}.
   * <ul>
   *   <li>When {@code Object}.</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#innerCreateAndSet(Class, Project)}
   */
  @Test
  public void testInnerCreateAndSet_whenJavaLangObject_thenProjectBuildListenersEmpty()
      throws IllegalAccessException, InstantiationException, NoSuchMethodException, InvocationTargetException {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    Class<Object> newclass = Object.class;
    Project project = new Project();

    // Act
    antTypeDefinition.innerCreateAndSet(newclass, project);

    // Assert that nothing has changed
    assertTrue(project.getBuildListeners().isEmpty());
  }

  /**
   * Test {@link AntTypeDefinition#sameDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#sameDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSameDefinition_givenAntTypeDefinition_whenNull_thenReturnFalse() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();

    // Act and Assert
    assertFalse(antTypeDefinition.sameDefinition(null, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#sameDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>Given {@link UnknownElement#UnknownElement(String)} with {@code Element Name}.</li>
   *   <li>When {@link AntTypeDefinition} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#sameDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSameDefinition_givenUnknownElementWithElementName_whenAntTypeDefinition() {
    // Arrange
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition preSetDefinition = new PreSetDefinition(parent, new UnknownElement("Element Name"));
    AntTypeDefinition other = new AntTypeDefinition();

    // Act and Assert
    assertFalse(preSetDefinition.sameDefinition(other, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#sameDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>When {@link UnknownElement#UnknownElement(String)} with {@code Element Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#sameDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSameDefinition_whenUnknownElementWithElementName_thenReturnFalse() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition other = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act and Assert
    assertFalse(antTypeDefinition.sameDefinition(other, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) AdaptToClass is {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSimilarDefinition_givenAntTypeDefinitionAdaptToClassIsObject() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    Class<Object> adaptToClass = Object.class;
    antTypeDefinition.setAdaptToClass(adaptToClass);
    antTypeDefinition.setClassName("Class Name");

    AntTypeDefinition other = new AntTypeDefinition();
    other.setClassName("Class Name");

    // Act and Assert
    assertFalse(antTypeDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) AdapterClass is {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSimilarDefinition_givenAntTypeDefinitionAdapterClassIsObject() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    Class<Object> adapterClass = Object.class;
    antTypeDefinition.setAdapterClass(adapterClass);
    antTypeDefinition.setClassName("Class Name");

    AntTypeDefinition other = new AntTypeDefinition();
    other.setClassName("Class Name");

    // Act and Assert
    assertFalse(antTypeDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) ClassLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSimilarDefinition_givenAntTypeDefinitionClassLoaderIsAntClassLoader() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassLoader(new AntClassLoader());
    antTypeDefinition.setClassName("Class Name");

    AntTypeDefinition other = new AntTypeDefinition();
    other.setClassName("Class Name");

    // Act and Assert
    assertFalse(antTypeDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor) Restrict is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSimilarDefinition_givenAntTypeDefinitionRestrictIsTrue() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setRestrict(true);
    antTypeDefinition.setClassName("Class Name");

    AntTypeDefinition other = new AntTypeDefinition();
    other.setClassName("Class Name");

    // Act and Assert
    assertFalse(antTypeDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSimilarDefinition_givenAntTypeDefinition_whenNull_thenReturnFalse() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();

    // Act and Assert
    assertFalse(antTypeDefinition.similarDefinition(null, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>Given {@link AntTypeDefinition} (default constructor).</li>
   *   <li>When {@link UnknownElement#UnknownElement(String)} with {@code Element Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSimilarDefinition_givenAntTypeDefinition_whenUnknownElementWithElementName() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    AntTypeDefinition parent = new AntTypeDefinition();
    PreSetDefinition other = new PreSetDefinition(parent, new UnknownElement("Element Name"));

    // Act and Assert
    assertFalse(antTypeDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>Given {@code Class Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSimilarDefinition_givenClassName_thenReturnTrue() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");

    AntTypeDefinition other = new AntTypeDefinition();
    other.setClassName("Class Name");

    // Act and Assert
    assertTrue(antTypeDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}.
   * <ul>
   *   <li>When {@link AntTypeDefinition} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntTypeDefinition#similarDefinition(AntTypeDefinition, Project)}
   */
  @Test
  public void testSimilarDefinition_whenAntTypeDefinition() {
    // Arrange
    AntTypeDefinition antTypeDefinition = new AntTypeDefinition();
    antTypeDefinition.setClassName("Class Name");
    AntTypeDefinition other = new AntTypeDefinition();

    // Act and Assert
    assertFalse(antTypeDefinition.similarDefinition(other, new Project()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link AntTypeDefinition}
   *   <li>{@link AntTypeDefinition#setAdaptToClass(Class)}
   *   <li>{@link AntTypeDefinition#setAdapterClass(Class)}
   *   <li>{@link AntTypeDefinition#setClassLoader(ClassLoader)}
   *   <li>{@link AntTypeDefinition#setClassName(String)}
   *   <li>{@link AntTypeDefinition#setName(String)}
   *   <li>{@link AntTypeDefinition#setRestrict(boolean)}
   *   <li>{@link AntTypeDefinition#getClassLoader()}
   *   <li>{@link AntTypeDefinition#getClassName()}
   *   <li>{@link AntTypeDefinition#getName()}
   *   <li>{@link AntTypeDefinition#isRestrict()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    AntTypeDefinition actualAntTypeDefinition = new AntTypeDefinition();
    Class<Object> adaptToClass = Object.class;
    actualAntTypeDefinition.setAdaptToClass(adaptToClass);
    Class<Object> adapterClass = Object.class;
    actualAntTypeDefinition.setAdapterClass(adapterClass);
    AntClassLoader classLoader = new AntClassLoader();
    actualAntTypeDefinition.setClassLoader(classLoader);
    actualAntTypeDefinition.setClassName("Class Name");
    actualAntTypeDefinition.setName("Name");
    actualAntTypeDefinition.setRestrict(true);
    ClassLoader actualClassLoader = actualAntTypeDefinition.getClassLoader();
    String actualClassName = actualAntTypeDefinition.getClassName();
    String actualName = actualAntTypeDefinition.getName();

    // Assert
    assertEquals("Class Name", actualClassName);
    assertEquals("Name", actualName);
    assertNotNull(actualClassLoader);
    assertTrue(actualAntTypeDefinition.isRestrict());
    assertSame(classLoader, actualClassLoader);
  }
}
