package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.taskdefs.optional.junit.JUnitTaskMirror.JUnitResultFormatterMirror;
import org.apache.tools.ant.taskdefs.optional.junit.JUnitTaskMirror.JUnitTestRunnerMirror;
import org.apache.tools.ant.taskdefs.optional.junit.JUnitTaskMirrorImpl.VmExitErrorTest;
import org.junit.Test;

public class JUnitTaskMirrorImplDiffblueTest {
  /**
   * Test {@link JUnitTaskMirrorImpl#addVmExit(JUnitTest, JUnitResultFormatterMirror, OutputStream, String, String)}.
   * <ul>
   *   <li>Then {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one toByteArray is empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTaskMirrorImpl#addVmExit(JUnitTest, JUnitResultFormatterMirror, OutputStream, String, String)}
   */
  @Test
  public void testAddVmExit_thenByteArrayOutputStreamWithOneToByteArrayIsEmptyArrayOfByte() throws Exception {
    // Arrange
    JUnitTaskMirrorImpl jUnitTaskMirrorImpl = new JUnitTaskMirrorImpl(new JUnitTask());
    JUnitTest test = new JUnitTest("Name");
    TearDownOnVmCrash aFormatter = new TearDownOnVmCrash();
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    jUnitTaskMirrorImpl.addVmExit(test, aFormatter, out, "Not all who wander are lost", "Test Case");

    // Assert that nothing has changed
    assertArrayEquals(new byte[]{}, out.toByteArray());
  }

  /**
   * Test {@link JUnitTaskMirrorImpl#addVmExit(JUnitTest, JUnitResultFormatterMirror, OutputStream, String, String)}.
   * <ul>
   *   <li>When {@link OutErrSummaryJUnitResultFormatter} (default constructor).</li>
   *   <li>Then array length is eighty-three.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTaskMirrorImpl#addVmExit(JUnitTest, JUnitResultFormatterMirror, OutputStream, String, String)}
   */
  @Test
  public void testAddVmExit_whenOutErrSummaryJUnitResultFormatter_thenArrayLengthIsEightyThree() throws Exception {
    // Arrange
    JUnitTaskMirrorImpl jUnitTaskMirrorImpl = new JUnitTaskMirrorImpl(new JUnitTask());
    JUnitTest test = new JUnitTest("Name");
    OutErrSummaryJUnitResultFormatter aFormatter = new OutErrSummaryJUnitResultFormatter();
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    jUnitTaskMirrorImpl.addVmExit(test, aFormatter, out, "Not all who wander are lost", "Test Case");

    // Assert
    assertEquals(83, out.toByteArray().length);
  }

  /**
   * Test {@link JUnitTaskMirrorImpl#addVmExit(JUnitTest, JUnitResultFormatterMirror, OutputStream, String, String)}.
   * <ul>
   *   <li>When {@link SummaryJUnitResultFormatter} (default constructor).</li>
   *   <li>Then array length is eighty-three.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTaskMirrorImpl#addVmExit(JUnitTest, JUnitResultFormatterMirror, OutputStream, String, String)}
   */
  @Test
  public void testAddVmExit_whenSummaryJUnitResultFormatter_thenArrayLengthIsEightyThree() throws Exception {
    // Arrange
    JUnitTaskMirrorImpl jUnitTaskMirrorImpl = new JUnitTaskMirrorImpl(new JUnitTask());
    JUnitTest test = new JUnitTest("Name");
    SummaryJUnitResultFormatter aFormatter = new SummaryJUnitResultFormatter();
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    jUnitTaskMirrorImpl.addVmExit(test, aFormatter, out, "Not all who wander are lost", "Test Case");

    // Assert
    assertEquals(83, out.toByteArray().length);
  }

  /**
   * Test {@link JUnitTaskMirrorImpl#newJUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean, AntClassLoader)}.
   * <ul>
   *   <li>When array of {@link String} with {@code Methods}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTaskMirrorImpl#newJUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean, AntClassLoader)}
   */
  @Test
  public void testNewJUnitTestRunner_whenArrayOfStringWithMethods() throws Exception {
    // Arrange
    JUnitTaskMirrorImpl jUnitTaskMirrorImpl = new JUnitTaskMirrorImpl(new JUnitTask());
    JUnitTest test = new JUnitTest("Name");

    // Act
    JUnitTestRunnerMirror actualNewJUnitTestRunnerResult = jUnitTaskMirrorImpl.newJUnitTestRunner(test,
        new String[]{"Methods"}, true, true, true, true, true, new AntClassLoader());

    // Assert
    assertTrue(actualNewJUnitTestRunnerResult instanceof JUnitTestRunner);
    assertEquals(0, actualNewJUnitTestRunnerResult.getRetCode());
  }

  /**
   * Test {@link JUnitTaskMirrorImpl#newJUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean, AntClassLoader)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTaskMirrorImpl#newJUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean, AntClassLoader)}
   */
  @Test
  public void testNewJUnitTestRunner_whenNull() throws Exception {
    // Arrange
    JUnitTaskMirrorImpl jUnitTaskMirrorImpl = new JUnitTaskMirrorImpl(new JUnitTask());
    JUnitTest test = new JUnitTest("Name");

    // Act
    JUnitTestRunnerMirror actualNewJUnitTestRunnerResult = jUnitTaskMirrorImpl.newJUnitTestRunner(test, null, true,
        true, true, true, true, new AntClassLoader());

    // Assert
    assertTrue(actualNewJUnitTestRunnerResult instanceof JUnitTestRunner);
    assertEquals(0, actualNewJUnitTestRunnerResult.getRetCode());
  }

  /**
   * Test {@link JUnitTaskMirrorImpl#newSummaryJUnitResultFormatter()}.
   * <p>
   * Method under test: {@link JUnitTaskMirrorImpl#newSummaryJUnitResultFormatter()}
   */
  @Test
  public void testNewSummaryJUnitResultFormatter() throws Exception {
    // Arrange, Act and Assert
    assertTrue((new JUnitTaskMirrorImpl(new JUnitTask()))
        .newSummaryJUnitResultFormatter() instanceof SummaryJUnitResultFormatter);
  }

  /**
   * Test VmExitErrorTest {@link VmExitErrorTest#countTestCases()}.
   * <p>
   * Method under test: {@link VmExitErrorTest#countTestCases()}
   */
  @Test
  public void testVmExitErrorTestCountTestCases() {
    // Arrange, Act and Assert
    assertEquals(1, (new VmExitErrorTest("A Message", new JUnitTest("Name"), "A Test Case")).countTestCases());
  }

  /**
   * Test VmExitErrorTest {@link VmExitErrorTest#getClassName()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VmExitErrorTest#getClassName()}
   */
  @Test
  public void testVmExitErrorTestGetClassName_givenJUnitTestWithName_thenReturnName() {
    // Arrange, Act and Assert
    assertEquals("Name", (new VmExitErrorTest("A Message", new JUnitTest("Name"), "A Test Case")).getClassName());
  }

  /**
   * Test VmExitErrorTest getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link VmExitErrorTest#VmExitErrorTest(String, JUnitTest, String)}
   *   <li>{@link VmExitErrorTest#toString()}
   *   <li>{@link VmExitErrorTest#getName()}
   * </ul>
   */
  @Test
  public void testVmExitErrorTestGettersAndSetters() {
    // Arrange and Act
    VmExitErrorTest actualVmExitErrorTest = new VmExitErrorTest("A Message", new JUnitTest("Name"), "A Test Case");
    String actualToStringResult = actualVmExitErrorTest.toString();

    // Assert
    assertEquals("A Test Case", actualVmExitErrorTest.getName());
    assertEquals("Name:A Test Case", actualToStringResult);
  }
}
