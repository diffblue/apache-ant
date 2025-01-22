package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Vector;
import org.junit.Test;

public class BaseTestDiffblueTest {
  /**
   * Test {@link BaseTest#getFiltertrace()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getFiltertrace()}
   */
  @Test
  public void testGetFiltertrace_givenJUnitTestWithName_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new JUnitTest("Name")).getFiltertrace());
  }

  /**
   * Test {@link BaseTest#getFiltertrace()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getFiltertrace()}
   */
  @Test
  public void testGetFiltertrace_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new JUnitTest("Name", true, true, false)).getFiltertrace());
  }

  /**
   * Test {@link BaseTest#setFork(boolean)}.
   * <p>
   * Method under test: {@link BaseTest#setFork(boolean)}
   */
  @Test
  public void testSetFork() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setFork(true);

    // Assert
    assertTrue(jUnitTest.getFork());
  }

  /**
   * Test {@link BaseTest#getFork()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name} Fork is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getFork()}
   */
  @Test
  public void testGetFork_givenJUnitTestWithNameForkIsTrue_thenReturnTrue() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    jUnitTest.setFork(true);

    // Act and Assert
    assertTrue(jUnitTest.getFork());
  }

  /**
   * Test {@link BaseTest#getFork()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getFork()}
   */
  @Test
  public void testGetFork_givenJUnitTestWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new JUnitTest("Name")).getFork());
  }

  /**
   * Test {@link BaseTest#setHaltonerror(boolean)}.
   * <p>
   * Method under test: {@link BaseTest#setHaltonerror(boolean)}
   */
  @Test
  public void testSetHaltonerror() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setHaltonerror(true);

    // Assert
    assertTrue(jUnitTest.getHaltonerror());
  }

  /**
   * Test {@link BaseTest#setHaltonfailure(boolean)}.
   * <p>
   * Method under test: {@link BaseTest#setHaltonfailure(boolean)}
   */
  @Test
  public void testSetHaltonfailure() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setHaltonfailure(true);

    // Assert
    assertTrue(jUnitTest.getHaltonfailure());
  }

  /**
   * Test {@link BaseTest#getHaltonerror()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getHaltonerror()}
   */
  @Test
  public void testGetHaltonerror_givenJUnitTestWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new JUnitTest("Name")).getHaltonerror());
  }

  /**
   * Test {@link BaseTest#getHaltonerror()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getHaltonerror()}
   */
  @Test
  public void testGetHaltonerror_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new JUnitTest("Name", true, true, true)).getHaltonerror());
  }

  /**
   * Test {@link BaseTest#getHaltonfailure()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getHaltonfailure()}
   */
  @Test
  public void testGetHaltonfailure_givenJUnitTestWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new JUnitTest("Name")).getHaltonfailure());
  }

  /**
   * Test {@link BaseTest#getHaltonfailure()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getHaltonfailure()}
   */
  @Test
  public void testGetHaltonfailure_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new JUnitTest("Name", true, true, true)).getHaltonfailure());
  }

  /**
   * Test {@link BaseTest#setIf(Object)} with {@code ifCondition}.
   * <ul>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} {@link BaseTest#ifProperty} is {@code If Condition}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#setIf(Object)}
   */
  @Test
  public void testSetIfWithIfCondition_thenJUnitTestWithNameIfPropertyIsIfCondition() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setIf((Object) "If Condition");

    // Assert
    assertEquals("If Condition", jUnitTest.ifProperty);
    assertEquals("If Condition", jUnitTest.getIfCondition());
  }

  /**
   * Test {@link BaseTest#setIf(Object)} with {@code ifCondition}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} IfCondition is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#setIf(Object)}
   */
  @Test
  public void testSetIfWithIfCondition_whenNull_thenJUnitTestWithNameIfConditionIsNull() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setIf((Object) null);

    // Assert that nothing has changed
    assertNull(jUnitTest.getIfCondition());
    assertNull(jUnitTest.ifProperty);
  }

  /**
   * Test {@link BaseTest#setIf(String)} with {@code propertyName}.
   * <ul>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} {@link BaseTest#ifProperty} is {@code Property Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#setIf(String)}
   */
  @Test
  public void testSetIfWithPropertyName_thenJUnitTestWithNameIfPropertyIsPropertyName() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setIf("Property Name");

    // Assert
    assertEquals("Property Name", jUnitTest.ifProperty);
    assertEquals("Property Name", jUnitTest.getIfCondition());
  }

  /**
   * Test {@link BaseTest#setIf(String)} with {@code propertyName}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} IfCondition is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#setIf(String)}
   */
  @Test
  public void testSetIfWithPropertyName_whenNull_thenJUnitTestWithNameIfConditionIsNull() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setIf((String) null);

    // Assert that nothing has changed
    assertNull(jUnitTest.getIfCondition());
    assertNull(jUnitTest.ifProperty);
  }

  /**
   * Test {@link BaseTest#getIfCondition()}.
   * <p>
   * Method under test: {@link BaseTest#getIfCondition()}
   */
  @Test
  public void testGetIfCondition() {
    // Arrange, Act and Assert
    assertNull((new JUnitTest("Name")).getIfCondition());
  }

  /**
   * Test {@link BaseTest#setUnless(String)} with {@code propertyName}.
   * <ul>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} UnlessCondition is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#setUnless(String)}
   */
  @Test
  public void testSetUnlessWithPropertyName_thenJUnitTestWithNameUnlessConditionIsNull() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setUnless((String) null);

    // Assert that nothing has changed
    assertNull(jUnitTest.getUnlessCondition());
    assertNull(jUnitTest.unlessProperty);
  }

  /**
   * Test {@link BaseTest#setUnless(String)} with {@code propertyName}.
   * <ul>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} {@link BaseTest#unlessProperty} is {@code Property Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#setUnless(String)}
   */
  @Test
  public void testSetUnlessWithPropertyName_thenJUnitTestWithNameUnlessPropertyIsPropertyName() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setUnless("Property Name");

    // Assert
    assertEquals("Property Name", jUnitTest.unlessProperty);
    assertEquals("Property Name", jUnitTest.getUnlessCondition());
  }

  /**
   * Test {@link BaseTest#setUnless(Object)} with {@code unlessCondition}.
   * <p>
   * Method under test: {@link BaseTest#setUnless(Object)}
   */
  @Test
  public void testSetUnlessWithUnlessCondition() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setUnless((Object) "Unless Condition");

    // Assert
    assertEquals("Unless Condition", jUnitTest.unlessProperty);
    assertEquals("Unless Condition", jUnitTest.getUnlessCondition());
  }

  /**
   * Test {@link BaseTest#setUnless(Object)} with {@code unlessCondition}.
   * <ul>
   *   <li>Then {@link JUnitTest#JUnitTest(String)} with {@code Name} UnlessCondition is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#setUnless(Object)}
   */
  @Test
  public void testSetUnlessWithUnlessCondition_thenJUnitTestWithNameUnlessConditionIsNull() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setUnless((Object) null);

    // Assert that nothing has changed
    assertNull(jUnitTest.getUnlessCondition());
    assertNull(jUnitTest.unlessProperty);
  }

  /**
   * Test {@link BaseTest#getUnlessCondition()}.
   * <p>
   * Method under test: {@link BaseTest#getUnlessCondition()}
   */
  @Test
  public void testGetUnlessCondition() {
    // Arrange, Act and Assert
    assertNull((new JUnitTest("Name")).getUnlessCondition());
  }

  /**
   * Test {@link BaseTest#addFormatter(FormatterElement)}.
   * <p>
   * Method under test: {@link BaseTest#addFormatter(FormatterElement)}
   */
  @Test
  public void testAddFormatter() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    FormatterElement elem = new FormatterElement();

    // Act
    jUnitTest.addFormatter(elem);

    // Assert
    Vector<FormatterElement> formatterElementList = jUnitTest.formatters;
    assertEquals(1, formatterElementList.size());
    FormatterElement[] formatters = jUnitTest.getFormatters();
    assertEquals(1, formatters.length);
    assertSame(elem, formatterElementList.get(0));
    assertSame(elem, formatters[0]);
  }

  /**
   * Test {@link BaseTest#setTodir(File)}.
   * <p>
   * Method under test: {@link BaseTest#setTodir(File)}
   */
  @Test
  public void testSetTodir() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setTodir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    File file = jUnitTest.destDir;
    assertEquals("test.txt", file.getName());
    assertTrue(file.isAbsolute());
    String expectedTodir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString();
    assertEquals(expectedTodir, jUnitTest.getTodir());
  }

  /**
   * Test {@link BaseTest#getTodir()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getTodir()}
   */
  @Test
  public void testGetTodir_givenJUnitTestWithName_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new JUnitTest("Name")).getTodir());
  }

  /**
   * Test {@link BaseTest#getTodir()}.
   * <ul>
   *   <li>Then return Property is {@code java.io.tmpdir} is array of {@link String} with {@code test.txt} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#getTodir()}
   */
  @Test
  public void testGetTodir_thenReturnPropertyIsJavaIoTmpdirIsArrayOfStringWithTestTxtToString() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    jUnitTest.setTodir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    String actualTodir = jUnitTest.getTodir();

    // Assert
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), actualTodir);
  }

  /**
   * Test {@link BaseTest#getFailureProperty()}.
   * <p>
   * Method under test: {@link BaseTest#getFailureProperty()}
   */
  @Test
  public void testGetFailureProperty() {
    // Arrange, Act and Assert
    assertNull((new JUnitTest("Name")).getFailureProperty());
  }

  /**
   * Test {@link BaseTest#setFailureProperty(String)}.
   * <p>
   * Method under test: {@link BaseTest#setFailureProperty(String)}
   */
  @Test
  public void testSetFailureProperty() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setFailureProperty("Failure Property");

    // Assert
    assertEquals("Failure Property", jUnitTest.getFailureProperty());
  }

  /**
   * Test {@link BaseTest#getErrorProperty()}.
   * <p>
   * Method under test: {@link BaseTest#getErrorProperty()}
   */
  @Test
  public void testGetErrorProperty() {
    // Arrange, Act and Assert
    assertNull((new JUnitTest("Name")).getErrorProperty());
  }

  /**
   * Test {@link BaseTest#setErrorProperty(String)}.
   * <p>
   * Method under test: {@link BaseTest#setErrorProperty(String)}
   */
  @Test
  public void testSetErrorProperty() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setErrorProperty("An error occurred");

    // Assert
    assertEquals("An error occurred", jUnitTest.getErrorProperty());
  }

  /**
   * Test {@link BaseTest#setSkipNonTests(boolean)}.
   * <p>
   * Method under test: {@link BaseTest#setSkipNonTests(boolean)}
   */
  @Test
  public void testSetSkipNonTests() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");

    // Act
    jUnitTest.setSkipNonTests(true);

    // Assert
    assertTrue(jUnitTest.isSkipNonTests());
  }

  /**
   * Test {@link BaseTest#isSkipNonTests()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name} SkipNonTests is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#isSkipNonTests()}
   */
  @Test
  public void testIsSkipNonTests_givenJUnitTestWithNameSkipNonTestsIsTrue_thenReturnTrue() {
    // Arrange
    JUnitTest jUnitTest = new JUnitTest("Name");
    jUnitTest.setSkipNonTests(true);

    // Act and Assert
    assertTrue(jUnitTest.isSkipNonTests());
  }

  /**
   * Test {@link BaseTest#isSkipNonTests()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseTest#isSkipNonTests()}
   */
  @Test
  public void testIsSkipNonTests_givenJUnitTestWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new JUnitTest("Name")).isSkipNonTests());
  }
}
