package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class WeakishReferenceDiffblueTest {
  /**
  * Method under test: {@link WeakishReference#WeakishReference(Object)}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertEquals("Reference", (new WeakishReference("Reference")).get());
  }

  /**
   * Method under test: {@link WeakishReference#createReference(Object)}
   */
  @Test
  public void testCreateReference() {
    // Arrange, Act and Assert
    assertEquals("Object", WeakishReference.createReference("Object").get());
  }

  /**
   * Method under test: {@link WeakishReference#get()}
   */
  @Test
  public void testGet() {
    // Arrange, Act and Assert
    assertEquals("Object", WeakishReference.createReference("Object").get());
  }

  /**
   * Method under test: {@link WeakishReference.HardReference#HardReference(Object)}
   */
  @Test
  public void testHardReferenceConstructor() {
    // Arrange, Act and Assert
    assertEquals("Object", (new WeakishReference.HardReference("Object")).get());
  }
}

