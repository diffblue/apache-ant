package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Hashtable;
import java.util.Properties;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class JUnitTestDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link JUnitTest#JUnitTest()}
   *   <li>{@link JUnitTest#setName(String)}
   *   <li>{@link JUnitTest#setOutfile(String)}
   *   <li>{@link JUnitTest#setRunTime(long)}
   *   <li>{@link JUnitTest#setThread(int)}
   *   <li>{@link JUnitTest#getName()}
   *   <li>{@link JUnitTest#getOutfile()}
   *   <li>{@link JUnitTest#getProperties()}
   *   <li>{@link JUnitTest#getRunTime()}
   *   <li>{@link JUnitTest#getThread()}
   *   <li>{@link JUnitTest#hasMethodsSpecified()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    JUnitTest actualJUnitTest = new JUnitTest();
    actualJUnitTest.setName("42");
    actualJUnitTest.setOutfile("42");
    actualJUnitTest.setRunTime(1L);
    actualJUnitTest.setThread(1);
    String actualName = actualJUnitTest.getName();
    String actualOutfile = actualJUnitTest.getOutfile();
    Properties actualProperties = actualJUnitTest.getProperties();
    long actualRunTime = actualJUnitTest.getRunTime();
    int actualThread = actualJUnitTest.getThread();
    boolean actualHasMethodsSpecifiedResult = actualJUnitTest.hasMethodsSpecified();

    // Assert
    assertEquals("42", actualName);
    assertEquals("42", actualOutfile);
    assertNull(actualJUnitTest.getIfCondition());
    assertNull(actualJUnitTest.getUnlessCondition());
    assertNull(actualJUnitTest.getErrorProperty());
    assertNull(actualJUnitTest.getFailureProperty());
    assertNull(actualProperties);
    assertEquals(1, actualThread);
    assertEquals(1L, actualRunTime);
    assertFalse(actualJUnitTest.getFork());
    assertFalse(actualJUnitTest.getHaltonerror());
    assertFalse(actualJUnitTest.getHaltonfailure());
    assertFalse(actualJUnitTest.isSkipNonTests());
    assertFalse(actualHasMethodsSpecifiedResult);
    assertTrue(actualJUnitTest.getFiltertrace());
  }

  /**
   * Test getters and setters.
   * <ul>
   *   <li>When {@code Name}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link JUnitTest#JUnitTest(String)}
   *   <li>{@link JUnitTest#setName(String)}
   *   <li>{@link JUnitTest#setOutfile(String)}
   *   <li>{@link JUnitTest#setRunTime(long)}
   *   <li>{@link JUnitTest#setThread(int)}
   *   <li>{@link JUnitTest#getName()}
   *   <li>{@link JUnitTest#getOutfile()}
   *   <li>{@link JUnitTest#getProperties()}
   *   <li>{@link JUnitTest#getRunTime()}
   *   <li>{@link JUnitTest#getThread()}
   *   <li>{@link JUnitTest#hasMethodsSpecified()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenName() {
    // Arrange and Act
    JUnitTest actualJUnitTest = new JUnitTest("Name");
    actualJUnitTest.setName("42");
    actualJUnitTest.setOutfile("42");
    actualJUnitTest.setRunTime(1L);
    actualJUnitTest.setThread(1);
    String actualName = actualJUnitTest.getName();
    String actualOutfile = actualJUnitTest.getOutfile();
    Properties actualProperties = actualJUnitTest.getProperties();
    long actualRunTime = actualJUnitTest.getRunTime();
    int actualThread = actualJUnitTest.getThread();
    boolean actualHasMethodsSpecifiedResult = actualJUnitTest.hasMethodsSpecified();

    // Assert
    assertEquals("42", actualName);
    assertEquals("42", actualOutfile);
    assertNull(actualJUnitTest.getIfCondition());
    assertNull(actualJUnitTest.getUnlessCondition());
    assertNull(actualJUnitTest.getErrorProperty());
    assertNull(actualJUnitTest.getFailureProperty());
    assertNull(actualProperties);
    assertEquals(1, actualThread);
    assertEquals(1L, actualRunTime);
    assertFalse(actualJUnitTest.getFork());
    assertFalse(actualJUnitTest.getHaltonerror());
    assertFalse(actualJUnitTest.getHaltonfailure());
    assertFalse(actualJUnitTest.isSkipNonTests());
    assertFalse(actualHasMethodsSpecifiedResult);
    assertTrue(actualJUnitTest.getFiltertrace());
  }

  /**
   * Test {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean)}.
   * <p>
   * Method under test: {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean)}
   */
  @Test
  public void testNewJUnitTest() {
    // Arrange and Act
    JUnitTest actualJUnitTest = new JUnitTest("Name", true, true, true);

    // Assert
    assertEquals("Name", actualJUnitTest.getName());
    assertNull(actualJUnitTest.getMethods());
    assertNull(actualJUnitTest.destDir);
    assertNull(actualJUnitTest.getIfCondition());
    assertNull(actualJUnitTest.getUnlessCondition());
    assertNull(actualJUnitTest.getErrorProperty());
    assertNull(actualJUnitTest.getFailureProperty());
    assertNull(actualJUnitTest.getTodir());
    assertNull(actualJUnitTest.getMethodsString());
    assertNull(actualJUnitTest.getOutfile());
    assertNull(actualJUnitTest.ifProperty);
    assertNull(actualJUnitTest.unlessProperty);
    assertNull(actualJUnitTest.getProperties());
    assertEquals(0, actualJUnitTest.getThread());
    assertEquals(0, actualJUnitTest.getFormatters().length);
    assertEquals(0L, actualJUnitTest.getRunTime());
    assertFalse(actualJUnitTest.getFork());
    assertFalse(actualJUnitTest.isSkipNonTests());
    assertFalse(actualJUnitTest.hasMethodsSpecified());
    assertTrue(actualJUnitTest.formatters.isEmpty());
    assertTrue(actualJUnitTest.getFiltertrace());
    assertTrue(actualJUnitTest.getHaltonerror());
    assertTrue(actualJUnitTest.getHaltonfailure());
  }

  /**
   * Test {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean, String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code Methods}.</li>
   *   <li>Then return MethodsString is {@code Methods}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean, String[])}
   */
  @Test
  public void testNewJUnitTest_whenArrayOfStringWithMethods_thenReturnMethodsStringIsMethods() {
    // Arrange and Act
    JUnitTest actualJUnitTest = new JUnitTest("Name", true, true, true, new String[]{"Methods"});

    // Assert
    assertEquals("Methods", actualJUnitTest.getMethodsString());
    assertTrue(actualJUnitTest.hasMethodsSpecified());
    assertArrayEquals(new String[]{"Methods"}, actualJUnitTest.getMethods());
  }

  /**
   * Test {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean, String[], int)}.
   * <ul>
   *   <li>When array of {@link String} with {@code Methods}.</li>
   *   <li>Then return MethodsString is {@code Methods}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean, String[], int)}
   */
  @Test
  public void testNewJUnitTest_whenArrayOfStringWithMethods_thenReturnMethodsStringIsMethods2() {
    // Arrange and Act
    JUnitTest actualJUnitTest = new JUnitTest("Name", true, true, true, new String[]{"Methods"}, 1);

    // Assert
    assertEquals("Methods", actualJUnitTest.getMethodsString());
    assertTrue(actualJUnitTest.hasMethodsSpecified());
    assertArrayEquals(new String[]{"Methods"}, actualJUnitTest.getMethods());
  }

  /**
   * Test {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean, String[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean, String[])}
   */
  @Test
  public void testNewJUnitTest_whenNull_thenReturnName() {
    // Arrange and Act
    JUnitTest actualJUnitTest = new JUnitTest("Name", true, true, true, null);

    // Assert
    assertEquals("Name", actualJUnitTest.getName());
    assertNull(actualJUnitTest.getMethods());
    assertNull(actualJUnitTest.destDir);
    assertNull(actualJUnitTest.getIfCondition());
    assertNull(actualJUnitTest.getUnlessCondition());
    assertNull(actualJUnitTest.getErrorProperty());
    assertNull(actualJUnitTest.getFailureProperty());
    assertNull(actualJUnitTest.getTodir());
    assertNull(actualJUnitTest.getMethodsString());
    assertNull(actualJUnitTest.getOutfile());
    assertNull(actualJUnitTest.ifProperty);
    assertNull(actualJUnitTest.unlessProperty);
    assertNull(actualJUnitTest.getProperties());
    assertEquals(0, actualJUnitTest.getThread());
    assertEquals(0, actualJUnitTest.getFormatters().length);
    assertEquals(0L, actualJUnitTest.getRunTime());
    assertFalse(actualJUnitTest.getFork());
    assertFalse(actualJUnitTest.isSkipNonTests());
    assertFalse(actualJUnitTest.hasMethodsSpecified());
    assertTrue(actualJUnitTest.formatters.isEmpty());
    assertTrue(actualJUnitTest.getFiltertrace());
    assertTrue(actualJUnitTest.getHaltonerror());
    assertTrue(actualJUnitTest.getHaltonfailure());
  }

  /**
   * Test {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean, String[], int)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#JUnitTest(String, boolean, boolean, boolean, String[], int)}
   */
  @Test
  public void testNewJUnitTest_whenNull_thenReturnName2() {
    // Arrange and Act
    JUnitTest actualJUnitTest = new JUnitTest("Name", true, true, true, null, 1);

    // Assert
    assertEquals("Name", actualJUnitTest.getName());
    assertNull(actualJUnitTest.getMethods());
    assertNull(actualJUnitTest.destDir);
    assertNull(actualJUnitTest.getIfCondition());
    assertNull(actualJUnitTest.getUnlessCondition());
    assertNull(actualJUnitTest.getErrorProperty());
    assertNull(actualJUnitTest.getFailureProperty());
    assertNull(actualJUnitTest.getTodir());
    assertNull(actualJUnitTest.getMethodsString());
    assertNull(actualJUnitTest.getOutfile());
    assertNull(actualJUnitTest.ifProperty);
    assertNull(actualJUnitTest.unlessProperty);
    assertNull(actualJUnitTest.getProperties());
    assertEquals(0, actualJUnitTest.getFormatters().length);
    assertEquals(0L, actualJUnitTest.getRunTime());
    assertEquals(1, actualJUnitTest.getThread());
    assertFalse(actualJUnitTest.getFork());
    assertFalse(actualJUnitTest.isSkipNonTests());
    assertFalse(actualJUnitTest.hasMethodsSpecified());
    assertTrue(actualJUnitTest.formatters.isEmpty());
    assertTrue(actualJUnitTest.getFiltertrace());
    assertTrue(actualJUnitTest.getHaltonerror());
    assertTrue(actualJUnitTest.getHaltonfailure());
  }

  /**
   * Test {@link JUnitTest#setMethods(String[])} with {@code String[]}.
   * <ul>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} MethodsString is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#setMethods(String[])}
   */
  @Test
  public void testSetMethodsWithString_thenJUnitTestWithNameMethodsStringIs42() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    String[] value = new String[]{"42"};

    // Act
    jUnitTest.setMethods(value);

    // Assert
    assertEquals("42", jUnitTest.getMethodsString());
    assertTrue(jUnitTest.hasMethodsSpecified());
    assertSame(value, jUnitTest.getMethods());
  }

  /**
   * Test {@link JUnitTest#setMethods(String)} with {@code String}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} MethodsString is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#setMethods(String)}
   */
  @Test
  public void testSetMethodsWithString_when42_thenJUnitTestWithNameMethodsStringIs42() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setMethods("42");

    // Assert
    assertEquals("42", jUnitTest.getMethodsString());
    assertTrue(jUnitTest.hasMethodsSpecified());
  }

  /**
   * Test {@link JUnitTest#setMethods(String)} with {@code String}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then not {@link JUnitTest#JUnitTest(String)} with {@code Name} hasMethodsSpecified.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#setMethods(String)}
   */
  @Test
  public void testSetMethodsWithString_whenNull_thenNotJUnitTestWithNameHasMethodsSpecified() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setMethods((String) null);

    // Assert that nothing has changed
    assertFalse(jUnitTest.hasMethodsSpecified());
  }

  /**
   * Test {@link JUnitTest#setMethods(String[])} with {@code String[]}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then not {@link JUnitTest#JUnitTest(String)} with {@code Name} hasMethodsSpecified.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#setMethods(String[])}
   */
  @Test
  public void testSetMethodsWithString_whenNull_thenNotJUnitTestWithNameHasMethodsSpecified2() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setMethods((String[]) null);

    // Assert that nothing has changed
    assertFalse(jUnitTest.hasMethodsSpecified());
  }

  /**
   * Test {@link JUnitTest#getMethods()}.
   * <p>
   * Method under test: {@link JUnitTest#getMethods()}
   */
  @Test
  public void testGetMethods() {
    // Arrange, Act and Assert
    assertNull((new JUnitTest("Name")).getMethods());
  }

  /**
   * Test {@link JUnitTest#getMethodsString()}.
   * <p>
   * Method under test: {@link JUnitTest#getMethodsString()}
   */
  @Test
  public void testGetMethodsString() {
    // Arrange, Act and Assert
    assertNull((new JUnitTest("Name")).getMethodsString());
  }

  /**
   * Test {@link JUnitTest#parseTestMethodNamesList(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#parseTestMethodNamesList(String)}
   */
  @Test
  public void testParseTestMethodNamesList_when42_thenThrowIllegalArgumentException() throws IllegalArgumentException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> JUnitTest.parseTestMethodNamesList("42"));
  }

  /**
   * Test {@link JUnitTest#parseTestMethodNamesList(String)}.
   * <ul>
   *   <li>When {@code ,}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#parseTestMethodNamesList(String)}
   */
  @Test
  public void testParseTestMethodNamesList_whenComma_thenThrowIllegalArgumentException()
      throws IllegalArgumentException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> JUnitTest.parseTestMethodNamesList(","));
  }

  /**
   * Test {@link JUnitTest#parseTestMethodNamesList(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#parseTestMethodNamesList(String)}
   */
  @Test
  public void testParseTestMethodNamesList_whenEmptyString_thenReturnArrayLengthIsZero()
      throws IllegalArgumentException {
    // Arrange, Act and Assert
    assertEquals(0, JUnitTest.parseTestMethodNamesList("").length);
  }

  /**
   * Test {@link JUnitTest#parseTestMethodNamesList(String)}.
   * <ul>
   *   <li>When {@code String[]}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#parseTestMethodNamesList(String)}
   */
  @Test
  public void testParseTestMethodNamesList_whenJavaLangString() throws IllegalArgumentException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> JUnitTest.parseTestMethodNamesList("java.lang.String[]"));
  }

  /**
   * Test {@link JUnitTest#parseTestMethodNamesList(String)}.
   * <ul>
   *   <li>When {@code Method Names}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#parseTestMethodNamesList(String)}
   */
  @Test
  public void testParseTestMethodNamesList_whenMethodNames_thenThrowIllegalArgumentException()
      throws IllegalArgumentException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> JUnitTest.parseTestMethodNamesList("Method Names"));
  }

  /**
   * Test {@link JUnitTest#parseTestMethodNamesList(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#parseTestMethodNamesList(String)}
   */
  @Test
  public void testParseTestMethodNamesList_whenNull_thenThrowIllegalArgumentException()
      throws IllegalArgumentException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> JUnitTest.parseTestMethodNamesList(null));
  }

  /**
   * Test {@link JUnitTest#parseTestMethodNamesList(String)}.
   * <ul>
   *   <li>When {@code Space in method name,}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#parseTestMethodNamesList(String)}
   */
  @Test
  public void testParseTestMethodNamesList_whenSpaceInMethodName() throws IllegalArgumentException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> JUnitTest.parseTestMethodNamesList("Space in method name,"));
  }

  /**
   * Test {@link JUnitTest#parseTestMethodNamesList(String)}.
   * <ul>
   *   <li>When {@code ,Space in method name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#parseTestMethodNamesList(String)}
   */
  @Test
  public void testParseTestMethodNamesList_whenSpaceInMethodName2() throws IllegalArgumentException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> JUnitTest.parseTestMethodNamesList(",Space in method name"));
  }

  /**
   * Test {@link JUnitTest#parseTestMethodNamesList(String)}.
   * <ul>
   *   <li>When {@code Space in method name,Space in method name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#parseTestMethodNamesList(String)}
   */
  @Test
  public void testParseTestMethodNamesList_whenSpaceInMethodNameSpaceInMethodName() throws IllegalArgumentException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> JUnitTest.parseTestMethodNamesList("Space in method name,Space in method name"));
  }

  /**
   * Test {@link JUnitTest#runCount()}.
   * <p>
   * Method under test: {@link JUnitTest#runCount()}
   */
  @Test
  public void testRunCount() {
    // Arrange, Act and Assert
    assertEquals(0L, (new JUnitTest("Name")).runCount());
  }

  /**
   * Test {@link JUnitTest#failureCount()}.
   * <p>
   * Method under test: {@link JUnitTest#failureCount()}
   */
  @Test
  public void testFailureCount() {
    // Arrange, Act and Assert
    assertEquals(0L, (new JUnitTest("Name")).failureCount());
  }

  /**
   * Test {@link JUnitTest#errorCount()}.
   * <p>
   * Method under test: {@link JUnitTest#errorCount()}
   */
  @Test
  public void testErrorCount() {
    // Arrange, Act and Assert
    assertEquals(0L, (new JUnitTest("Name")).errorCount());
  }

  /**
   * Test {@link JUnitTest#skipCount()}.
   * <p>
   * Method under test: {@link JUnitTest#skipCount()}
   */
  @Test
  public void testSkipCount() {
    // Arrange, Act and Assert
    assertEquals(0L, (new JUnitTest("Name")).skipCount());
  }

  /**
   * Test {@link JUnitTest#setProperties(Hashtable)}.
   * <p>
   * Method under test: {@link JUnitTest#setProperties(Hashtable)}
   */
  @Test
  public void testSetProperties() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    Hashtable<Object, Object> p = new Hashtable<>();

    // Act
    jUnitTest.setProperties(p);

    // Assert
    assertEquals(p, jUnitTest.getProperties());
  }

  /**
   * Test {@link JUnitTest#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(jUnitTest.shouldRun(p));
  }

  /**
   * Test {@link JUnitTest#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenJavaLangObject() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("Adding reference: ", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(jUnitTest.shouldRun(p));
  }

  /**
   * Test {@link JUnitTest#shouldRun(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_givenTarget_whenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(jUnitTest.shouldRun(p));
  }

  /**
   * Test {@link JUnitTest#shouldRun(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_whenNull() {
    // Arrange, Act and Assert
    assertTrue((new JUnitTest("Name")).shouldRun(null));
  }

  /**
   * Test {@link JUnitTest#shouldRun(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#shouldRun(Project)}
   */
  @Test
  public void testShouldRun_whenProject() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act and Assert
    assertTrue(jUnitTest.shouldRun(new Project()));
  }

  /**
   * Test {@link JUnitTest#getFormatters()}.
   * <p>
   * Method under test: {@link JUnitTest#getFormatters()}
   */
  @Test
  public void testGetFormatters() {
    // Arrange, Act and Assert
    assertEquals(0, (new JUnitTest("Name")).getFormatters().length);
  }

  /**
   * Test {@link JUnitTest#addFormattersTo(Vector)}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name}.</li>
   *   <li>Then {@link Vector#Vector()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#addFormattersTo(Vector)}
   */
  @Test
  public void testAddFormattersTo_givenJUnitTestWithName_thenVectorEmpty() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    Vector<FormatterElement> v = new Vector<>();

    // Act
    jUnitTest.addFormattersTo(v);

    // Assert that nothing has changed
    assertTrue(v.isEmpty());
  }

  /**
   * Test {@link JUnitTest#addFormattersTo(Vector)}.
   * <ul>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} {@link BaseTest#formatters} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#addFormattersTo(Vector)}
   */
  @Test
  public void testAddFormattersTo_thenJUnitTestWithNameFormattersSizeIsOne() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    FormatterElement elem = new FormatterElement();
    jUnitTest.addFormatter(elem);
    Vector<FormatterElement> v = new Vector<>();

    // Act
    jUnitTest.addFormattersTo(v);

    // Assert
    Vector<FormatterElement> formatterElementList = jUnitTest.formatters;
    assertEquals(1, formatterElementList.size());
    FormatterElement[] formatters = jUnitTest.getFormatters();
    assertEquals(1, formatters.length);
    assertEquals(jUnitTest.formatters, v);
    assertSame(elem, formatterElementList.get(0));
    assertSame(elem, formatters[0]);
  }

  /**
   * Test {@link JUnitTest#addFormattersTo(Vector)}.
   * <ul>
   *   <li>Then {@link Vector#Vector()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#addFormattersTo(Vector)}
   */
  @Test
  public void testAddFormattersTo_thenVectorSizeIsTwo() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    jUnitTest.addFormatter(new FormatterElement());
    FormatterElement elem = new FormatterElement();
    jUnitTest.addFormatter(elem);
    Vector<FormatterElement> v = new Vector<>();

    // Act
    jUnitTest.addFormattersTo(v);

    // Assert
    assertEquals(2, v.size());
    Vector<FormatterElement> formatterElementList = jUnitTest.formatters;
    assertEquals(2, formatterElementList.size());
    FormatterElement[] formatters = jUnitTest.getFormatters();
    assertEquals(2, formatters.length);
    assertSame(elem, v.get(1));
    assertSame(elem, formatterElementList.get(1));
    assertSame(elem, formatters[1]);
  }

  /**
   * Test {@link JUnitTest#clone()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name} Properties is {@link Hashtable#Hashtable()}.</li>
   *   <li>Then return Properties Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#clone()}
   */
  @Test
  public void testClone_givenJUnitTestWithNamePropertiesIsHashtable_thenReturnPropertiesEmpty() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    jUnitTest.setProperties(new Hashtable<>());

    // Act
    Object actualCloneResult = jUnitTest.clone();

    // Assert
    assertTrue(actualCloneResult instanceof JUnitTest);
    assertEquals("Name", ((JUnitTest) actualCloneResult).getName());
    assertNull(((JUnitTest) actualCloneResult).getMethods());
    assertNull(((JUnitTest) actualCloneResult).destDir);
    assertNull(((JUnitTest) actualCloneResult).getIfCondition());
    assertNull(((JUnitTest) actualCloneResult).getUnlessCondition());
    assertNull(((JUnitTest) actualCloneResult).getErrorProperty());
    assertNull(((JUnitTest) actualCloneResult).getFailureProperty());
    assertNull(((JUnitTest) actualCloneResult).getTodir());
    assertNull(((JUnitTest) actualCloneResult).getMethodsString());
    assertNull(((JUnitTest) actualCloneResult).getOutfile());
    assertNull(((JUnitTest) actualCloneResult).ifProperty);
    assertNull(((JUnitTest) actualCloneResult).unlessProperty);
    assertEquals(0, ((JUnitTest) actualCloneResult).getThread());
    assertEquals(0, ((JUnitTest) actualCloneResult).getFormatters().length);
    assertEquals(0L, ((JUnitTest) actualCloneResult).getRunTime());
    assertFalse(((JUnitTest) actualCloneResult).getFork());
    assertFalse(((JUnitTest) actualCloneResult).getHaltonerror());
    assertFalse(((JUnitTest) actualCloneResult).getHaltonfailure());
    assertFalse(((JUnitTest) actualCloneResult).isSkipNonTests());
    assertFalse(((JUnitTest) actualCloneResult).hasMethodsSpecified());
    assertTrue(((JUnitTest) actualCloneResult).getProperties().isEmpty());
    assertTrue(((JUnitTest) actualCloneResult).formatters.isEmpty());
    assertTrue(((JUnitTest) actualCloneResult).getFiltertrace());
  }

  /**
   * Test {@link JUnitTest#clone()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name}.</li>
   *   <li>Then return Properties is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTest#clone()}
   */
  @Test
  public void testClone_givenJUnitTestWithName_thenReturnPropertiesIsNull() {
    // Arrange and Act
    Object actualCloneResult = (new JUnitTest("Name")).clone();

    // Assert
    assertTrue(actualCloneResult instanceof JUnitTest);
    assertEquals("Name", ((JUnitTest) actualCloneResult).getName());
    assertNull(((JUnitTest) actualCloneResult).getMethods());
    assertNull(((JUnitTest) actualCloneResult).destDir);
    assertNull(((JUnitTest) actualCloneResult).getIfCondition());
    assertNull(((JUnitTest) actualCloneResult).getUnlessCondition());
    assertNull(((JUnitTest) actualCloneResult).getErrorProperty());
    assertNull(((JUnitTest) actualCloneResult).getFailureProperty());
    assertNull(((JUnitTest) actualCloneResult).getTodir());
    assertNull(((JUnitTest) actualCloneResult).getMethodsString());
    assertNull(((JUnitTest) actualCloneResult).getOutfile());
    assertNull(((JUnitTest) actualCloneResult).ifProperty);
    assertNull(((JUnitTest) actualCloneResult).unlessProperty);
    assertNull(((JUnitTest) actualCloneResult).getProperties());
    assertEquals(0, ((JUnitTest) actualCloneResult).getThread());
    assertEquals(0, ((JUnitTest) actualCloneResult).getFormatters().length);
    assertEquals(0L, ((JUnitTest) actualCloneResult).getRunTime());
    assertFalse(((JUnitTest) actualCloneResult).getFork());
    assertFalse(((JUnitTest) actualCloneResult).getHaltonerror());
    assertFalse(((JUnitTest) actualCloneResult).getHaltonfailure());
    assertFalse(((JUnitTest) actualCloneResult).isSkipNonTests());
    assertFalse(((JUnitTest) actualCloneResult).hasMethodsSpecified());
    assertTrue(((JUnitTest) actualCloneResult).formatters.isEmpty());
    assertTrue(((JUnitTest) actualCloneResult).getFiltertrace());
  }
}
