package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.RedirectorElement;
import org.junit.Test;

public class VerifyJarDiffblueTest {
  /**
   * Test {@link VerifyJar#execute()}.
   * <ul>
   *   <li>Given {@link VerifyJar} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VerifyJar#execute()}
   */
  @Test
  public void testExecute_givenVerifyJar_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new VerifyJar()).execute());
  }

  /**
   * Test {@link VerifyJar#beginExecution()}.
   * <ul>
   *   <li>Given {@link VerifyJar} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link VerifyJar#beginExecution()}
   */
  @Test
  public void testBeginExecution_givenVerifyJar() {
    // Arrange
    VerifyJar verifyJar = new VerifyJar();

    // Act
    verifyJar.beginExecution();

    // Assert
    RedirectorElement redirector = verifyJar.getRedirector();
    assertNull(redirector.getDescription());
    assertNull(verifyJar.storepass);
    assertNull(redirector.getProject());
    assertNull(redirector.getRefid());
    assertFalse(redirector.isReference());
  }

  /**
   * Test {@link VerifyJar#beginExecution()}.
   * <ul>
   *   <li>Given {@link VerifyJar} (default constructor) Storepass is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VerifyJar#beginExecution()}
   */
  @Test
  public void testBeginExecution_givenVerifyJarStorepassIsFoo() {
    // Arrange
    VerifyJar verifyJar = new VerifyJar();
    verifyJar.setStorepass("foo");

    // Act
    verifyJar.beginExecution();

    // Assert
    RedirectorElement redirector = verifyJar.getRedirector();
    assertNull(redirector.getDescription());
    assertNull(verifyJar.storepass);
    assertNull(redirector.getProject());
    assertNull(redirector.getRefid());
    assertFalse(redirector.isReference());
  }

  /**
   * Test new {@link VerifyJar} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link VerifyJar}
   */
  @Test
  public void testNewVerifyJar() {
    // Arrange and Act
    VerifyJar actualVerifyJar = new VerifyJar();

    // Assert
    assertNull(actualVerifyJar.jar);
    assertNull(actualVerifyJar.getDescription());
    assertNull(actualVerifyJar.getTaskName());
    assertNull(actualVerifyJar.getTaskType());
    assertNull(actualVerifyJar.alias);
    assertNull(actualVerifyJar.keypass);
    assertNull(actualVerifyJar.keystore);
    assertNull(actualVerifyJar.maxMemory);
    assertNull(actualVerifyJar.storepass);
    assertNull(actualVerifyJar.storetype);
    assertNull(actualVerifyJar.getProject());
    assertNull(actualVerifyJar.getOwningTarget());
    assertNull(actualVerifyJar.getRedirector());
    assertFalse(actualVerifyJar.hasResources());
    assertFalse(actualVerifyJar.strict);
    assertFalse(actualVerifyJar.verbose);
    assertTrue(actualVerifyJar.filesets.isEmpty());
  }
}
